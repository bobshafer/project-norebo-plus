#define _GNU_SOURCE

#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

static uint32_t sysreq_exec(uint32_t n);
static uint32_t risc_time(void);
static void risc_leds(uint32_t n);

#define PathEnv "NOREBO_PATH"
#define InnerCore "InnerCore"

#define MemBytes (8 * 1024 * 1024)
#define StackOrg 0x80000
#define MaxFiles 500
#define NameLength 32

struct File {
  FILE *f;
  char name[NameLength];
  bool registered;
};

static uint8_t mem[MemBytes];
static uint32_t sysarg[3], sysres;
static uint32_t nargc;
static char **nargv;
static struct File files[MaxFiles];
static DIR *dir;

uint32_t PC;
uint32_t R[16];
uint32_t H;
bool Z, N, C, V;

void risc_run(void);

enum {
  MOV,
  LSL,
  ASR,
  ROR,
  AND,
  ANN,
  IOR,
  XOR,
  ADD,
  SUB,
  MUL,
  DIV,
  FAD,
  FSB,
  FML,
  FDV,
};

static void risc_single_step(void);
static uint32_t fp_add(uint32_t x, uint32_t y, bool u, bool v);
static uint32_t fp_mul(uint32_t x, uint32_t y);
static uint32_t fp_div(uint32_t x, uint32_t y);
static struct idiv {
  uint32_t quot, rem;
} idiv(uint32_t x, uint32_t y, bool signed_div);

inline static void risc_set_register(int reg, uint32_t value) {
  R[reg] = value;
  Z = value == 0;
  N = (int32_t)value < 0;
}
inline static uint32_t le32_to_host(uint8_t *ptr) {
  return ptr[0] | (ptr[1] << 8) | (ptr[2] << 16) | (ptr[3] << 24);
}

inline static uint32_t mem_read_word(uint32_t adr) {
  if (adr >= MemBytes - 3) {
    fprintf(stderr, "Memory read out of bounds (address %#08x)\n", adr);
    exit(1);
  }
  return le32_to_host(mem + adr);
}

inline static uint8_t mem_read_byte(uint32_t adr) {
  if (adr >= MemBytes) {
    fprintf(stderr, "Memory read out of bounds (address %#08x)\n", adr);
    exit(1);
  }
  return mem[adr];
}

inline static void mem_write_word(uint32_t adr, uint32_t val) {
  if (adr >= MemBytes - 3) {
    fprintf(stderr, "Memory write out of bounds (address %#08x)\n", adr);
    exit(1);
  }
  uint8_t *ptr = mem + adr;
  ptr[0] = (uint8_t)val;
  ptr[1] = (uint8_t)(val >> 8);
  ptr[2] = (uint8_t)(val >> 16);
  ptr[3] = (uint8_t)(val >> 24);
}

inline static void mem_write_byte(uint32_t adr, uint32_t val) {
  if (adr >= MemBytes) {
    fprintf(stderr, "Memory read out of bounds (address %#08x)\n", adr);
    exit(1);
  }
  mem[adr] = (uint8_t)val;
}

inline static void mem_check_range(uint32_t adr, uint32_t siz,
                                   const char *proc) {
  if (adr >= MemBytes || MemBytes - adr < siz) {
    fprintf(stderr, "%s: Memory access out of bounds\n", proc);
    exit(1);
  }
}
inline static uint32_t io_read_word(uint32_t adr) {
  switch (-adr / 4) {
  /* carried over from oberon */
  case 64 / 4:
    return risc_time();
  case 56 / 4:
    return getchar();
  case 52 / 4:
    return 3;
  /* norebo interface */
  case 16 / 4:
    return sysarg[2];
  case 12 / 4:
    return sysarg[1];
  case 8 / 4:
    return sysarg[0];
  case 4 / 4:
    return sysres;
  default:
    fprintf(stderr, "Unimplemented read of I/O address %d\n", adr);
    exit(1);
    return 0;
  }
}

inline static void io_write_word(uint32_t adr, uint32_t val) {
  switch (-adr / 4) {
  /* carried over from oberon */
  case 60 / 4:
    risc_leds(val);
    break;
  case 56 / 4:
    putchar(val);
    if (val == '\n' || val == '\r')
      fflush(stdout);
    break;
  /* norebo interface */
  case 16 / 4:
    sysarg[2] = val;
    break;
  case 12 / 4:
    sysarg[1] = val;
    break;
  case 8 / 4:
    sysarg[0] = val;
    break;
  case 4 / 4:
    sysres = sysreq_exec(val);
    // printf("%d(%d,%d,%d)=>%d\n",val,sysarg[0],sysarg[1],sysarg[2],sysres);
    break;
  default:
    fprintf(stderr, "Unimplemented write of I/O address %d\n", adr);
    exit(1);
  }
}

inline static uint32_t cpu_read_program(uint32_t adr) {
  return mem_read_word(adr * 4);
}

inline static uint32_t cpu_read_word(uint32_t adr) {
  return (int32_t)adr >= 0 ? mem_read_word(adr) : io_read_word(adr);
}

inline static uint32_t cpu_read_byte(uint32_t adr) {
  return (int32_t)adr >= 0 ? mem_read_byte(adr) : io_read_word(adr);
}

inline static void cpu_write_word(uint32_t adr, uint32_t val) {
  (int32_t) adr >= 0 ? mem_write_word(adr, val) : io_write_word(adr, val);
}

inline static void cpu_write_byte(uint32_t adr, uint8_t val) {
  (int32_t) adr >= 0 ? mem_write_byte(adr, val) : io_write_word(adr, val);
}

/* Boot */

inline static bool read_uint32(uint32_t *v, FILE *f) {
  uint8_t buf[4];
  if (fread(&buf, 1, 4, f) != 4) {
    return false;
  }
  *v = le32_to_host(buf);
  return true;
}

void risc_run(void) {
  for (;;) {
    risc_single_step();
  }
}

static void risc_single_step(void) {
  uint32_t ir = cpu_read_program(PC);
  PC++;

  const uint32_t pbit = 0x80000000;
  const uint32_t qbit = 0x40000000;
  const uint32_t ubit = 0x20000000;
  const uint32_t vbit = 0x10000000;

  if ((ir & pbit) == 0) {
    // Register instructions
    uint32_t a = (ir & 0x0F000000) >> 24;
    uint32_t b = (ir & 0x00F00000) >> 20;
    uint32_t op = (ir & 0x000F0000) >> 16;
    uint32_t im = ir & 0x0000FFFF;
    uint32_t c = ir & 0x0000000F;

    uint32_t a_val, b_val, c_val;
    b_val = R[b];
    if ((ir & qbit) == 0) {
      c_val = R[c];
    } else if ((ir & vbit) == 0) {
      c_val = im;
    } else {
      c_val = 0xFFFF0000 | im;
    }

    switch (op) {
    case MOV: {
      if ((ir & ubit) == 0) {
        a_val = c_val;
      } else if ((ir & qbit) != 0) {
        a_val = c_val << 16;
      } else if ((ir & vbit) != 0) {
        a_val = 0xD0 | // ???
                (N * 0x80000000U) | (Z * 0x40000000U) | (C * 0x20000000U) |
                (V * 0x10000000U);
      } else {
        a_val = H;
      }
      break;
    }
    case LSL: {
      a_val = b_val << (c_val & 31);
      break;
    }
    case ASR: {
      a_val = ((int32_t)b_val) >> (c_val & 31);
      break;
    }
    case ROR: {
      a_val = (b_val >> (c_val & 31)) | (b_val << (-c_val & 31));
      break;
    }
    case AND: {
      a_val = b_val & c_val;
      break;
    }
    case ANN: {
      a_val = b_val & ~c_val;
      break;
    }
    case IOR: {
      a_val = b_val | c_val;
      break;
    }
    case XOR: {
      a_val = b_val ^ c_val;
      break;
    }
    case ADD: {
      a_val = b_val + c_val;
      if ((ir & ubit) != 0) {
        a_val += C;
      }
      C = a_val < b_val;
      V = ((a_val ^ c_val) & (a_val ^ b_val)) >> 31;
      break;
    }
    case SUB: {
      a_val = b_val - c_val;
      if ((ir & ubit) != 0) {
        a_val -= C;
      }
      C = a_val > b_val;
      V = ((b_val ^ c_val) & (a_val ^ b_val)) >> 31;
      break;
    }
    case MUL: {
      uint64_t tmp;
      if ((ir & ubit) == 0) {
        tmp = (int64_t)(int32_t)b_val * (int64_t)(int32_t)c_val;
      } else {
        tmp = (uint64_t)b_val * (uint64_t)c_val;
      }
      a_val = (uint32_t)tmp;
      H = (uint32_t)(tmp >> 32);
      break;
    }
    case DIV: {
      if ((int32_t)c_val > 0) {
        if ((ir & ubit) == 0) {
          a_val = (int32_t)b_val / (int32_t)c_val;
          H = (int32_t)b_val % (int32_t)c_val;
          if ((int32_t)H < 0) {
            a_val--;
            H += c_val;
          }
        } else {
          a_val = b_val / c_val;
          H = b_val % c_val;
        }
      } else {
        struct idiv q = idiv(b_val, c_val, ir & ubit);
        a_val = q.quot;
        H = q.rem;
      }
      break;
    }
    case FAD: {
      a_val = fp_add(b_val, c_val, ir & ubit, ir & vbit);
      break;
    }
    case FSB: {
      a_val = fp_add(b_val, c_val ^ 0x80000000, ir & ubit, ir & vbit);
      break;
    }
    case FML: {
      a_val = fp_mul(b_val, c_val);
      break;
    }
    case FDV: {
      a_val = fp_div(b_val, c_val);
      break;
    }
    default: {
      abort(); // unreachable
    }
    }
    risc_set_register(a, a_val);
  } else if ((ir & qbit) == 0) {
    // Memory instructions
    uint32_t a = (ir & 0x0F000000) >> 24;
    uint32_t b = (ir & 0x00F00000) >> 20;
    int32_t off = ir & 0x000FFFFF;
    off = (off ^ 0x00080000) - 0x00080000; // sign-extend

    uint32_t address = R[b] + off;
    if ((ir & ubit) == 0) {
      uint32_t a_val;
      if ((ir & vbit) == 0) {
        a_val = cpu_read_word(address);
      } else {
        a_val = cpu_read_byte(address);
      }
      risc_set_register(a, a_val);
    } else {
      if ((ir & vbit) == 0) {
        cpu_write_word(address, R[a]);
      } else {
        cpu_write_byte(address, ((unsigned char)R[a]) % 256);
      }
    }
  } else {
    // Branch instructions
    bool t = (ir >> 27) & 1;
    switch ((ir >> 24) & 7) {
    case 0:
      t ^= N;
      break;
    case 1:
      t ^= Z;
      break;
    case 2:
      t ^= C;
      break;
    case 3:
      t ^= V;
      break;
    case 4:
      t ^= C | Z;
      break;
    case 5:
      t ^= N ^ V;
      break;
    case 6:
      t ^= (N ^ V) | Z;
      break;
    case 7:
      t ^= true;
      break;
    default:
      abort(); // unreachable
    }
    if (t) {
      if ((ir & vbit) != 0) {
        risc_set_register(15, PC * 4);
      }
      if ((ir & ubit) == 0) {
        uint32_t c = ir & 0x0000000F;
        PC = R[c] / 4;
      } else {
        int32_t off = ir & 0x00FFFFFF;
        off = (off ^ 0x00800000) - 0x00800000; // sign-extend
        PC = PC + off;
      }
    }
  }
}

static uint32_t fp_add(uint32_t x, uint32_t y, bool u, bool v) {
  bool xs = (x & 0x80000000) != 0;
  uint32_t xe;
  int32_t x0;
  if (!u) {
    xe = (x >> 23) & 0xFF;
    uint32_t xm = ((x & 0x7FFFFF) << 1) | 0x1000000;
    x0 = (int32_t)(xs ? -xm : xm);
  } else {
    xe = 150;
    x0 = (int32_t)(x & 0x00FFFFFF) << 8 >> 7;
  }

  bool ys = (y & 0x80000000) != 0;
  uint32_t ye = (y >> 23) & 0xFF;
  uint32_t ym = ((y & 0x7FFFFF) << 1);
  if (!u && !v)
    ym |= 0x1000000;
  int32_t y0 = (int32_t)(ys ? -ym : ym);

  uint32_t e0;
  int32_t x3, y3;
  if (ye > xe) {
    uint32_t shift = ye - xe;
    e0 = ye;
    x3 = shift > 31 ? x0 >> 31 : x0 >> shift;
    y3 = y0;
  } else {
    uint32_t shift = xe - ye;
    e0 = xe;
    x3 = x0;
    y3 = shift > 31 ? y0 >> 31 : y0 >> shift;
  }

  uint32_t sum = ((xs << 26) | (xs << 25) | (x3 & 0x01FFFFFF)) +
                 ((ys << 26) | (ys << 25) | (y3 & 0x01FFFFFF));

  uint32_t s = (((sum & (1 << 26)) ? -sum : sum) + 1) & 0x07FFFFFF;

  uint32_t e1 = e0 + 1;
  uint32_t t3 = s >> 1;
  if ((s & 0x3FFFFFC) != 0) {
    while ((t3 & (1 << 24)) == 0) {
      t3 <<= 1;
      e1--;
    }
  } else {
    t3 <<= 24;
    e1 -= 24;
  }

  if (v) {
    return (int32_t)(sum << 5) >> 6;
  } else if ((x & 0x7FFFFFFF) == 0) {
    return !u ? y : 0;
  } else if ((y & 0x7FFFFFFF) == 0) {
    return x;
  } else if ((t3 & 0x01FFFFFF) == 0 || (e1 & 0x100) != 0) {
    return 0;
  } else {
    return ((sum & 0x04000000) << 5) | (e1 << 23) | ((t3 >> 1) & 0x7FFFFF);
  }
}

static uint32_t fp_mul(uint32_t x, uint32_t y) {
  uint32_t sign = (x ^ y) & 0x80000000;
  uint32_t xe = (x >> 23) & 0xFF;
  uint32_t ye = (y >> 23) & 0xFF;

  uint32_t xm = (x & 0x7FFFFF) | 0x800000;
  uint32_t ym = (y & 0x7FFFFF) | 0x800000;
  uint64_t m = (uint64_t)xm * ym;

  uint32_t e1 = (xe + ye) - 127;
  uint32_t z0;
  if ((m & (1ULL << 47)) != 0) {
    e1++;
    z0 = ((m >> 23) + 1) & 0xFFFFFF;
  } else {
    z0 = ((m >> 22) + 1) & 0xFFFFFF;
  }

  if (xe == 0 || ye == 0) {
    return 0;
  } else if ((e1 & 0x100) == 0) {
    return sign | ((e1 & 0xFF) << 23) | (z0 >> 1);
  } else if ((e1 & 0x80) == 0) {
    return sign | (0xFF << 23) | (z0 >> 1);
  } else {
    return 0;
  }
}

static uint32_t fp_div(uint32_t x, uint32_t y) {
  uint32_t sign = (x ^ y) & 0x80000000;
  uint32_t xe = (x >> 23) & 0xFF;
  uint32_t ye = (y >> 23) & 0xFF;

  uint32_t xm = (x & 0x7FFFFF) | 0x800000;
  uint32_t ym = (y & 0x7FFFFF) | 0x800000;
  uint32_t q1 = (uint32_t)(xm * (1ULL << 25) / ym);

  uint32_t e1 = (xe - ye) + 126;
  uint32_t q2;
  if ((q1 & (1 << 25)) != 0) {
    e1++;
    q2 = (q1 >> 1) & 0xFFFFFF;
  } else {
    q2 = q1 & 0xFFFFFF;
  }
  uint32_t q3 = q2 + 1;

  if (xe == 0) {
    return 0;
  } else if (ye == 0) {
    return sign | (0xFF << 23);
  } else if ((e1 & 0x100) == 0) {
    return sign | ((e1 & 0xFF) << 23) | (q3 >> 1);
  } else if ((e1 & 0x80) == 0) {
    return sign | (0xFF << 23) | (q2 >> 1);
  } else {
    return 0;
  }
}

static struct idiv idiv(uint32_t x, uint32_t y, bool signed_div) {
  bool sign = ((int32_t)x < 0) & signed_div;
  uint32_t x0 = sign ? -x : x;

  uint64_t RQ = x0;
  for (int S = 0; S < 32; ++S) {
    uint32_t w0 = (uint32_t)(RQ >> 31);
    uint32_t w1 = w0 - y;
    if ((int32_t)w1 < 0) {
      RQ = ((uint64_t)w0 << 32) | ((RQ & 0x7FFFFFFFU) << 1);
    } else {
      RQ = ((uint64_t)w1 << 32) | ((RQ & 0x7FFFFFFFU) << 1) | 1;
    }
  }

  struct idiv d = {(uint32_t)RQ, (uint32_t)(RQ >> 32)};
  if (sign) {
    d.quot = -d.quot;
    if (d.rem) {
      d.quot -= 1;
      d.rem = y - d.rem;
    }
  }
  return d;
}
/* Norebo module */

static uint32_t norebo_halt(uint32_t ec, uint32_t _2, uint32_t _3) { exit(ec); }

static uint32_t norebo_argc(uint32_t _1, uint32_t _2, uint32_t _3) {
  return nargc;
}

static uint32_t norebo_argv(uint32_t idx, uint32_t adr, uint32_t siz) {
  mem_check_range(adr, siz, "Norebo.Argv");
  if (idx < nargc) {
    if (siz > 0) {
      strncpy((char *)mem + adr, nargv[idx], siz - 1);
      mem[adr + siz - 1] = 0;
    }
    return (uint32_t)strlen(nargv[idx]);
  } else {
    return -1;
  }
}

static uint32_t os_getenv(uint32_t env, uint32_t adr, uint32_t siz) {
  mem_check_range(adr, siz, "OS.Getenv");
  char *p = getenv((char *)mem + env);
  if (p != NULL && siz > 0) {
    strncpy((char *)mem + adr, p, siz - 1);
    mem[adr + siz - 1] = 0;
    return 1;
  }
  return 0;
}

static bool files_get_name(char *name, uint32_t adr);

static uint32_t norebo_trap(uint32_t trap, uint32_t name_adr, uint32_t pos) {
  char message[100];
  switch (trap) {
  case 1:
    strcpy(message, "array index out of range");
    break;
  case 2:
    strcpy(message, "type guard failure");
    break;
  case 3:
    strcpy(message, "array or string copy overflow");
    break;
  case 4:
    strcpy(message, "access via NIL pointer");
    break;
  case 5:
    strcpy(message, "illegal procedure call");
    break;
  case 6:
    strcpy(message, "integer division by zero");
    break;
  case 7:
    strcpy(message, "assertion violated");
    break;
  default:
    sprintf(message, "unknown trap %d\n", trap);
    break;
  }
  char name[NameLength];
  if (!files_get_name(name, name_adr)) {
    strcpy(name, "(unknown)");
  }
  fprintf(stderr, "%s at %s pos %d\n", message, name, pos);
  exit(100 + trap);
  return 0;
}

/* Files module */

static FILE *path_fopen(const char *path, const char *filename,
                        const char *mode) {
  if (!path) {
    errno = ENOENT;
    return NULL;
  }
  const char *sep = strchr(path, ';') ? ";" : ":";
  FILE *f = NULL;
  do {
    size_t part_len = strcspn(path, sep);
    if (part_len == 0) {
      f = fopen(filename, mode);
    } else {
      char *buf = NULL;
      int r = asprintf(&buf, "%.*s/%s", (int)part_len, path, filename);
      if (r < 0) {
        fprintf(stderr, "Failed to asprintf()");
        exit(1);
      }
      f = fopen(buf, mode);
      free(buf);
    }
    path += part_len + 1;
  } while (f == NULL && errno == ENOENT && path[-1] != 0);
  return f;
}

static bool files_check_name(char *name) {
  for (int i = 0; i < NameLength; ++i) {
    char ch = name[i];
    if (ch == 0) {
      return true;
    } else if (!((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') ||
                 (i > 0 && (ch == '.' || (ch >= '0' && ch <= '9'))))) {
      return false;
    }
  }
  return false;
}

static bool files_get_name(char *name, uint32_t adr) {
  mem_check_range(adr, NameLength, "Files.GetName");
  memcpy(name, mem + adr, NameLength);
  return files_check_name(name);
}

static int files_allocate(const char *name, bool registered) {
  for (int h = 0; h < MaxFiles; ++h) {
    if (!files[h].f) {
      strncpy(files[h].name, name, NameLength);
      files[h].registered = registered;
      return h;
    }
  }
  fprintf(stderr, "Files.Allocate: Too many open files");
  exit(1);
  return 0;
}

static void files_check_handle(int h, const char *proc) {
  if (h < 0 || h >= MaxFiles || !files[h].f) {
    fprintf(stderr, "%s: Invalid file handle\n", proc);
    exit(1);
  }
}

static uint32_t files_new(uint32_t adr, uint32_t _2, uint32_t _3) {
  char name[NameLength];
  if (!files_get_name(name, adr)) {
    return -1;
  }
  int h = files_allocate(name, false);
  files[h].f = tmpfile();
  if (!files[h].f) {
    fprintf(stderr, "Files.New: %s\n", name);
    exit(1);
  }
  return h;
}

static uint32_t files_old(uint32_t adr, uint32_t _2, uint32_t _3) {
  char name[NameLength];
  if (!files_get_name(name, adr)) {
    return -1;
  }
  int h = files_allocate(name, true);
  files[h].f = fopen(name, "r+b");
  if (!files[h].f) {
    files[h].f = path_fopen(getenv(PathEnv), name, "rb");
  }
  if (!files[h].f) {
    files[h] = (struct File){0};
    return -1;
  }
  return h;
}

static uint32_t files_register(uint32_t h, uint32_t _2, uint32_t _3) {
  files_check_handle(h, "Files.Register");
  if (!files[h].registered && files[h].name[0]) {
    FILE *old = files[h].f;
    files[h].f = fopen(files[h].name, "w+b");
    if (!files[h].f) {
      fprintf(stderr, "Can't create file %s\n", files[h].name);
      exit(1);
    }
    errno = 0;
    fseek(old, 0, SEEK_SET);
    char buf[8192];
    size_t in = fread(buf, 1, sizeof(buf), old);
    while (in != 0) {
      size_t out = fwrite(buf, 1, in, files[h].f);
      if (in != out) {
        fprintf(stderr, "Can't write file %s\n", files[h].name);
        exit(1);
      }
      in = fread(buf, 1, sizeof(buf), old);
    }
    fclose(old);
    if (fflush(files[h].f) != 0) {
      fprintf(stderr, "Can't flush file %s\n", files[h].name);
      exit(1);
    }
    files[h].registered = true;
  }
  return 0;
}

static uint32_t files_close(uint32_t h, uint32_t _2, uint32_t _3) {
  files_check_handle(h, "Files.Close");
  fclose(files[h].f);
  files[h] = (struct File){0};
  return 0;
}

static uint32_t files_seek(uint32_t h, uint32_t pos, uint32_t whence) {
  files_check_handle(h, "Files.Seek");
  return fseek(files[h].f, pos, whence);
}

static uint32_t files_tell(uint32_t h, uint32_t _2, uint32_t _3) {
  files_check_handle(h, "Files.Tell");
  return (uint32_t)ftell(files[h].f);
}

static uint32_t files_read(uint32_t h, uint32_t adr, uint32_t siz) {
  files_check_handle(h, "Files.Read");
  mem_check_range(adr, siz, "Files.Read");
  size_t r = fread(mem + adr, 1, siz, files[h].f);
  memset(mem + adr + r, 0, siz - r);
  return (uint32_t)r;
}

static uint32_t files_write(uint32_t h, uint32_t adr, uint32_t siz) {
  files_check_handle(h, "Files.Write");
  mem_check_range(adr, siz, "Files.Write");
  return (uint32_t)fwrite(mem + adr, 1, siz, files[h].f);
}

static uint32_t files_length(uint32_t h, uint32_t _2, uint32_t _3) {
  files_check_handle(h, "Files.Length");
  fflush(files[h].f);
  struct stat s;
  int r = fstat(fileno(files[h].f), &s);
  if (r < 0) {
    fprintf(stderr, "Files.Length");
    exit(1);
  }
  return (uint32_t)s.st_size;
}

static uint32_t time_to_oberon(time_t t) {
  struct tm tm = {0};
  localtime_r(&t, &tm);
  return ((tm.tm_year % 100) * 0x4000000) | (tm.tm_mon * 0x400000) |
         (tm.tm_mday * 0x20000) | (tm.tm_hour * 0x1000) | (tm.tm_min * 0x40) |
         tm.tm_sec;
}

static uint32_t files_date(uint32_t h, uint32_t _2, uint32_t _3) {
  files_check_handle(h, "Files.Date");
  fflush(files[h].f);
  if (files[h].registered) {
    struct stat s;
    int r = fstat(fileno(files[h].f), &s);
    if (r < 0) {
      fprintf(stderr, "Files.Date");
      exit(1);
    }
    return time_to_oberon(s.st_mtime);
  } else {
    return time_to_oberon(time(NULL));
  }
}

static uint32_t files_delete(uint32_t adr, uint32_t _2, uint32_t _3) {
  char name[NameLength];
  if (!files_get_name(name, adr) || !name[0]) {
    return -1;
  }
  if (remove(name) < 0) {
    return -1;
  }
  return 0;
}

static uint32_t files_purge(uint32_t h, uint32_t _2, uint32_t _3) {
  fprintf(stderr, "Files.Purge not implemented");
  exit(1);
}

static uint32_t files_rename(uint32_t adr_old, uint32_t adr_new, uint32_t _3) {
  char old_name[NameLength], new_name[NameLength];
  if (!files_get_name(old_name, adr_old) || !old_name[0] ||
      !files_get_name(new_name, adr_new) || !new_name[0]) {
    return -1;
  }
  if (rename(old_name, new_name) < 0) {
    return -1;
  }
  return 0;
}

/* FileDir module */

static uint32_t filedir_enumerate_begin(uint32_t _1, uint32_t _2, uint32_t _3) {
  if (dir) {
    closedir(dir);
  }
  dir = opendir(".");
  if (!dir) {
    fprintf(stderr, "FileDir.BeginEnumerate");
    exit(1);
  }
  return 0;
}

static uint32_t filedir_enumerate_next(uint32_t adr, uint32_t _2, uint32_t _3) {
  mem_check_range(adr, NameLength, "FileDir.EnumerateNext");
  struct dirent *ent = NULL;
  if (dir) {
    do {
      ent = readdir(dir);
    } while (ent && !files_check_name(ent->d_name));
  }
  if (!ent) {
    mem_write_byte(adr, 0);
    return -1;
  }
  assert(strlen(ent->d_name) < NameLength);
  strncpy((char *)mem + adr, ent->d_name, NameLength);
  return 0;
}

static uint32_t filedir_enumerate_end(uint32_t _1, uint32_t _2, uint32_t _3) {
  if (dir) {
    closedir(dir);
    dir = NULL;
  }
  return 0;
}

/* I/O dispatch */

typedef uint32_t (*sysreq_fn)(uint32_t, uint32_t, uint32_t);

static sysreq_fn sysreq_table[] = {
    [1] = norebo_halt,
    [2] = norebo_argc,
    [3] = norebo_argv,
    [4] = norebo_trap,

    [11] = files_new,
    [12] = files_old,
    [13] = files_register,
    [14] = files_close,
    [15] = files_seek,
    [16] = files_tell,
    [17] = files_read,
    [18] = files_write,
    [19] = files_length,
    [20] = files_date,
    [21] = files_delete,
    [22] = files_purge,
    [23] = files_rename,

    [31] = filedir_enumerate_begin,
    [32] = filedir_enumerate_next,
    [33] = filedir_enumerate_end,

    [41] = os_getenv,
};

static const uint32_t sysreq_cnt =
    sizeof(sysreq_table) / sizeof(sysreq_table[0]);

static uint32_t sysreq_exec(uint32_t n) {
  if (n >= sysreq_cnt || !sysreq_table[n]) {
    fprintf(stderr, "Unimplemented sysreq %d\n", n);
    exit(1);
  }
  return sysreq_table[n](sysarg[0], sysarg[1], sysarg[2]);
}

static uint32_t risc_time(void) {
  struct timeval tv = {0};
  gettimeofday(&tv, NULL);
  return (uint32_t)(tv.tv_sec * 1000 + tv.tv_usec / 1000);
}

static void risc_leds(uint32_t n) {
  static char buf[] = "[LEDs: 76543210]\n";
  for (int i = 0; i < 8; ++i) {
    buf[14 - i] = (n & (1 << i)) ? (char)('0' + i) : '-';
  }
  fputs(buf, stderr);
}

/* CPU glue */

/* Boot */

static void load_inner_core(void) {
  FILE *f = fopen(InnerCore, "rb");
  if (!f) {
    f = path_fopen(getenv(PathEnv), InnerCore, "rb");
  }
  if (!f) {
    fprintf(stderr, "Can't load " InnerCore);
    exit(1);
  }

  uint32_t siz, adr;
  if (!read_uint32(&siz, f)) {
    goto fail;
  }
  while (siz != 0) {
    if (!read_uint32(&adr, f)) {
      goto fail;
    }
    mem_check_range(adr, siz, InnerCore);
    if (fread(mem + adr, 1, siz, f) != siz) {
      goto fail;
    }
    if (!read_uint32(&siz, f)) {
      goto fail;
    }
  }
  fclose(f);
  return;

fail:
  if (feof(f)) {
    fprintf(stderr, "Unexpected end of file while reading " InnerCore);
    exit(1);
  }
  fprintf(stderr, "Error while reading " InnerCore);
  exit(1);
}

int main(int argc, char *argv[]) {
  nargc = argc - 1;
  nargv = argv + 1;

  load_inner_core();
  mem_write_word(12, MemBytes);
  mem_write_word(24, StackOrg);
  PC = 0;
  R[12] = 0x20;
  R[14] = StackOrg;
  risc_run();

  return 0;
}
