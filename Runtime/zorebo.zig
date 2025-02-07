const std = @import("std");

const PathEnv = "NOREBO_PATH";
const InnerCore = "InnerCore";

const MemBytes = 8 * 1024 * 1024;
const StackOrg = 0x80000;
const MaxFiles = 500;
const NameLength = 32;

const mem = [_]u8{0} ** MemBytes;
var sysarg: [3]u32 = undefined;
var sysres: u32 = undefined;
var nargc: u32 = undefined;
var nargv: [][*c]u8 = undefined;
var files: [MaxFiles]struct {
    f: *std.fs.File,
    name: [NameLength]u8,
    registered: bool,
} = undefined;
var dir: *std.fs.Dir = undefined;

var PC: u32 = undefined;
var R: [16]u32 = undefined;
var H: u32 = undefined;
var Z: bool = undefined;
var N: bool = undefined;
var C: bool = undefined;
var V: bool = undefined;

const MOV = 0;
const LSL = 1;
const ASR = 2;
const ROR = 3;
const AND = 4;
const ANN = 5;
const IOR = 6;
const XOR = 7;
const ADD = 8;
const SUB = 9;
const MUL = 10;
const DIV = 11;
const FAD = 12;
const FSB = 13;
const FML = 14;
const FDV = 15;

fn risc_single_step() void {
    const ir = cpu_read_program(PC);
    PC += 1;

    const pbit = 0x80000000;
    const qbit = 0x40000000;
    const ubit = 0x20000000;
    const vbit = 0x10000000;

    if ((ir & pbit) == 0) {
        // Register instructions
        const a = (ir & 0x0F000000) >> 24;
        const b = (ir & 0x00F00000) >> 20;
        const op = (ir & 0x000F0000) >> 16;
        const im = ir & 0x0000FFFF;
        const c = ir & 0x0000000F;

        var a_val: u32 = undefined;
        const b_val = R[b];
        const c_val = if ((ir & qbit) == 0) {
            R[c];
        } else if ((ir & vbit) == 0) {
            im;
        } else {
            0xFFFF0000 | im;
        };

        switch (op) {
            MOV => {
                if ((ir & ubit) == 0) {
                    a_val = c_val;
                } else if ((ir & qbit) != 0) {
                    a_val = c_val << 16;
                } else if ((ir & vbit) != 0) {
                    a_val = 0xD0 |
                        (if (N)
                    {
                        0x80000000;
                    } else {
                        0;
                    }) |
                        (if (Z)
                    {
                        0x40000000;
                    } else {
                        0;
                    }) |
                        (if (C)
                    {
                        0x20000000;
                    } else {
                        0;
                    }) |
                        (if (V)
                    {
                        0x10000000;
                    } else {
                        0;
                    });
                } else {
                    a_val = H;
                }
            },
            LSL => {
                a_val = b_val << (c_val & 31);
            },
            ASR => {
                a_val = (@as(i32, @intCast(b_val))) >> (c_val & 31);
            },
            ROR => {
                a_val = (b_val >> (c_val & 31)) | (b_val << (-c_val & 31));
            },
            AND => {
                a_val = b_val & c_val;
            },
            ANN => {
                a_val = b_val & !c_val;
            },
            IOR => {
                a_val = b_val | c_val;
            },
            XOR => {
                a_val = b_val ^ c_val;
            },
            ADD => {
                a_val = b_val + c_val;
                if ((ir & ubit) != 0) {
                    a_val += C;
                }
                C = a_val < b_val;
                V = ((a_val ^ c_val) & (a_val ^ b_val)) >> 31;
            },
            SUB => {
                a_val = b_val - c_val;
                if ((ir & ubit) != 0) {
                    a_val -= C;
                }
                C = a_val > b_val;
                V = ((b_val ^ c_val) & (a_val ^ b_val)) >> 31;
            },
            MUL => {
                var tmp: u64 = undefined;
                if ((ir & ubit) == 0) {
                    tmp = @as(u64, @intCast(@as(i32, @intCast(b_val)))) * @as(u64, @intCast(@as(i32, @intCast(c_val))));
                } else {
                    tmp = @as(u64, @intCast(b_val)) * @as(u64, @intCast(c_val));
                }
                a_val = @as(u32, @intCast(tmp));
                H = @as(u32, @intCast(tmp >> 32));
            },
            DIV => {
                if (@as(i32, @intCast(c_val)) > 0) {
                    if ((ir & ubit) == 0) {
                        a_val = @as(i32, @intCast(b_val)) / @as(i32, @intCast(c_val));
                        H = @as(i32, @intCast(b_val)) % @as(i32, @intCast(c_val));
                        if (@as(i32, @intCast(H)) < 0) {
                            a_val -= 1;
                            H += c_val;
                        }
                    } else {
                        a_val = b_val / c_val;
                        H = b_val % c_val;
                    }
                } else {
                    const q = idiv(b_val, c_val, ir & ubit);
                    a_val = q.quot;
                    H = q.rem;
                }
            },
            FAD => {
                a_val = fp_add(b_val, c_val, ir & ubit, ir & vbit);
            },
            FSB => {
                a_val = fp_add(b_val, c_val ^ 0x80000000, ir & ubit, ir & vbit);
            },
            FML => {
                a_val = fp_mul(b_val, c_val);
            },
            FDV => {
                a_val = fp_div(b_val, c_val);
            },
            else => {
                @panic("unreachable");
            },
        }
        risc_set_register(a, a_val);
    } else if ((ir & qbit) == 0) {

        // Memory instructions
        const a = (ir & 0x0F000000) >> 24;
        const b = (ir & 0x00F00000) >> 20;
        const off0 = ir & 0x000FFFFF;
        const off = (@as(i32, @intCast(off0)) ^ 0x00080000) - 0x00080000; // sign-extend

        const address = R[b] + off;
        if ((ir & ubit) == 0) {
            var a_val: u32 = undefined;
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
                cpu_write_byte(address, @as(u8, @intCast(R[a])) % 256);
            }
        }
    } else {
        // Branch instructions
        const t = (ir >> 27) & 1;
        switch ((ir >> 24) & 7) {
            0 => {
                t ^= N;
            },
            1 => {
                t ^= Z;
            },
            2 => {
                t ^= C;
            },
            3 => {
                t ^= V;
            },
            4 => {
                t ^= C | Z;
            },
            5 => {
                t ^= N ^ V;
            },
            6 => {
                t ^= (N ^ V) | Z;
            },
            7 => {
                t ^= true;
            },
            else => {
                @panic("unreachable");
            },
        }
        if (t) {
            if ((ir & vbit) != 0) {
                risc_set_register(15, PC * 4);
            }
            if ((ir & ubit) == 0) {
                const c = ir & 0x0000000F;
                PC = R[c] / 4;
            } else {
                const off0 = ir & 0x00FFFFFF;
                const off = @as(i32, @intCast(off0 ^ 0x00800000)) - 0x00800000; // sign-extend
                PC = PC + off;
            }
        }
    }
}

fn fp_add(x: u32, y: u32, u: bool, v: bool) u32 {
    const xs = (x & 0x80000000) != 0;
    const xe = (x >> 23) & 0xFF;
    const x0 = if (!u) {
        const xm = ((x & 0x7FFFFF) << 1) | 0x1000000;
        (if (@as(i32, @intCast(xs)) > 0) {
            -xm;
        } else {
            xm;
        });
    } else {
        150;
    };

    const ys = (y & 0x80000000) != 0;
    const ye = (y >> 23) & 0xFF;
    const ym0 = ((y & 0x7FFFFF) << 1);
    const ym = if (!u and !v) {
        ym0 | 0x1000000;
    } else {
        ym0;
    };
    const y0 = if (ys) {
        -ym;
    } else {
        ym;
    };

    var e0: u32 = undefined;
    var x3: i32 = undefined;
    var y3: i32 = undefined;
    if (ye > xe) {
        const shift = ye - xe;
        e0 = ye;
        x3 = if (shift > 31) {
            x0 >> 31;
        } else {
            x0 >> shift;
        };
        y3 = y0;
    } else {
        const shift = xe - ye;
        e0 = xe;
        x3 = x0;
        y3 = if (shift > 31) {
            y0 >> 31;
        } else {
            y0 >> shift;
        };
    }

    const sum = ((xs << 26) | (xs << 25) | (x3 & 0x01FFFFFF)) +
        ((ys << 26) | (ys << 25) | (y3 & 0x01FFFFFF));

    const s = ((if ((sum & (1 << 26)) > 0) {
        -sum;
    } else {
        sum;
    }) + 1) & 0x07FFFFFF;

    var e1 = e0 + 1;
    var t3 = s >> 1;
    if ((s & 0x3FFFFFC) != 0) {
        while ((t3 & (1 << 24)) == 0) {
            t3 <<= 1;
            e1 -= 1;
        }
    } else {
        t3 <<= 24;
        e1 -= 24;
    }

    if (v) {
        return @as(u32, @intCast(sum << 5)) >> 6;
    } else if ((x & 0x7FFFFFFF) == 0) {
        return (if (!u) y else 0);
    } else if ((y & 0x7FFFFFFF) == 0) {
        return x;
    } else if (((t3 & 0x01FFFFFF) == 0) || (e1 & 0x100) != 0) {
        return 0;
    } else {
        return ((sum & 0x04000000) << 5) | (e1 << 23) | ((t3 >> 1) & 0x7FFFFF);
    }
}

fn fp_mul(x: u32, y: u32) u32 {
    const sign = (x ^ y) & 0x80000000;
    const xe = (x >> 23) & 0xFF;
    const ye = (y >> 23) & 0xFF;

    const xm = (x & 0x7FFFFF) | 0x800000;
    const ym = (y & 0x7FFFFF) | 0x800000;
    const m = @as(u64, @intCast(xm)) * @as(u64, @intCast(ym));

    const e1 = (xe + ye) - 127;
    var z0: u32 = undefined;
    if ((m & (1 << 47)) != 0) {
        e1 += 1;
        z0 = ((m >> 23) + 1) & 0xFFFFFF;
    } else {
        z0 = ((m >> 22) + 1) & 0xFFFFFF;
    }

    if ((xe == 0) || ye == 0) {
        return 0;
    } else if ((e1 & 0x100) == 0) {
        return sign | ((e1 & 0xFF) << 23) | (z0 >> 1);
    } else if ((e1 & 0x80) == 0) {
        return sign | (0xFF << 23) | (z0 >> 1);
    } else {
        return 0;
    }
}

fn fp_div(x: u32, y: u32) u32 {
    const sign = (x ^ y) & 0x80000000;
    const xe = (x >> 23) & 0xFF;
    const ye = (y >> 23) & 0xFF;

    const xm = (x & 0x7FFFFF) | 0x800000;
    const ym = (y & 0x7FFFFF) | 0x800000;
    const m = @as(u64, @intCast(xm)) * @as(u64, @intCast(ym));

    const e1 = (xe - ye) + 127;
    var z0: u32 = undefined;
    if ((m & (1 << 47)) != 0) {
        e1 += 1;
        z0 = ((m >> 23) + 1) & 0xFFFFFF;
    } else {
        z0 = ((m >> 22) + 1) & 0xFFFFFF;
    }

    if ((xe == 0) || ye == 0) {
        return 0;
    } else if ((e1 & 0x100) == 0) {
        return sign | ((e1 & 0xFF) << 23) | (z0 >> 1);
    } else if ((e1 & 0x80) == 0) {
        return sign | (0xFF << 23) | (z0 >> 1);
    } else {
        return 0;
    }
}

const idiv_struct = struct {
    quot: u32,
    rem: u32,
};

fn idiv(x: u32, y: u32, signed_div: bool) idiv_struct {
    if (signed_div) {
        const xi = @as(i32, @intCast(x));
        const yi = @as(i32, @intCast(y));
        return idiv_struct{
            .quot = @as(u32, @intCast(xi / yi)),
            .rem = @as(u32, @intCast(xi % yi)),
        };
    } else {
        return idiv_struct{
            .quot = x / y,
            .rem = x % y,
        };
    }
}

fn risc_set_register(reg: u32, value: u32) void {
    R[reg] = value;
    Z = value == 0;
    N = (@as(i32, @intCast(value)) < 0);
}

fn le32_to_host(ptr: [*]u8) u32 {
    return ptr[0] | (ptr[1] << 8) | (ptr[2] << 16) | (ptr[3] << 24);
}

fn mem_read_word(adr: u32) u32 {
    if (adr >= MemBytes - 3) {
        @panic(std.fmt("Memory read out of bounds (address {x})", adr));
    }
    return le32_to_host(mem + adr);
}

fn mem_read_byte(adr: u32) u8 {
    if (adr >= MemBytes) {
        @panic(std.fmt("Memory read out of bounds (address {x})", adr));
    }
    return mem[adr];
}

fn mem_write_word(adr: u32, val: u32) void {
    if (adr >= MemBytes - 3) {
        @panic(std.fmt("Memory write out of bounds (address {x})", adr));
    }
    const ptr = mem + adr;
    ptr[0] = @as(u8, @intCast(val));
    ptr[1] = @as(u8, @intCast(val >> 8));
    ptr[2] = @as(u8, @intCast(val >> 16));
    ptr[3] = @as(u8, @intCast(val >> 24));
}

fn mem_write_byte(adr: u32, val: u32) void {
    if (adr >= MemBytes) {
        @panic(std.fmt("Memory read out of bounds (address {x})", adr));
    }
    mem[adr] = @as(u8, @intCast(val));
}

fn mem_check_range(adr: u32, siz: u32, proc: *u8) void {
    if ((adr >= MemBytes) || (MemBytes - adr) < siz) {
        @panic(std.fmt("{s}: Memory access out of bounds", proc));
    }
}

fn io_read_word(adr: u32) u32 {
    switch (-@as(i32, @intCast(adr)) / 4) {
        // carried over from oberon
        64 / 4 => return risc_time(),
        56 / 4 => return @as(u32, @intCast(std.io.getchar())),
        52 / 4 => return 3,
        // norebo interface
        16 / 4 => return sysarg[2],
        12 / 4 => return sysarg[1],
        8 / 4 => return sysarg[0],
        4 / 4 => return sysres,
        else => {
            @panic(std.fmt("Unimplemented read of I/O address {d}", adr));
        },
    }
}

fn io_write_word(adr: u32, val: u32) void {
    switch (-@as(i32, @intCast(adr)) / 4) {
        // carried over from oberon
        60 / 4 => risc_leds(val),
        56 / 4 => {
            std.io.putchar(@as(i32, @intCast(val)));
            if ((val == '\n') || val == '\r') {
                std.io.flush(std.io.getStdOut());
            }
        },
        // norebo interface
        16 / 4 => sysarg[2] = val,
        12 / 4 => sysarg[1] = val,
        8 / 4 => sysarg[0] = val,
        4 / 4 => sysres = sysreq_exec(val),
        else => {
            @panic(std.fmt("Unimplemented write of I/O address {d}", adr));
        },
    }
}

fn cpu_read_program(adr: u32) u32 {
    return mem_read_word(adr * 4);
}

fn cpu_read_word(adr: u32) u32 {
    return if (@as(i32, @intCast(adr)) >= 0) {
        mem_read_word(adr);
    } else {
        io_read_word(adr);
    };
}

fn cpu_read_byte(adr: u32) u32 {
    return if (@as(i32, @intCast(adr)) >= 0) {
        mem_read_byte(adr);
    } else {
        io_read_word(adr);
    };
}

fn cpu_write_word(adr: u32, val: u32) void {
    if (@as(i32, @intCast(adr)) >= 0) {
        mem_write_word(adr, val);
    } else {
        io_write_word(adr, val);
    }
}

fn cpu_write_byte(adr: u32, val: u8) void {
    if (@as(i32, @intCast(adr)) >= 0) {
        mem_write_byte(adr, val);
    } else {
        io_write_word(adr, @as(u32, @intCast(val)));
    }
}

// Boot

fn read_uint32(v: *u32, f: *std.fs.File) bool {
    var buf: [4]u8 = undefined;
    if (f.read(buf) != 4) {
        return false;
    }
    v.* = le32_to_host(&buf);
    return true;
}

fn risc_run() void {
    while (true) {
        risc_single_step();
    }
}

// Norebo module

fn norebo_halt(ec: u32, _: u32, _: u32) u32 {
    std.os.exit(ec);
}

fn norebo_argc(_: u32, _: u32, _: u32) u32 {
    return nargc;
}

fn norebo_argv(idx: u32, adr: u32, siz: u32) u32 {
    mem_check_range(adr, siz, "Norebo.Argv");
    if (idx < nargc) {
        if (siz > 0) {
            std.mem.copy(mem + adr, nargv[idx], siz - 1);
            mem[adr + siz - 1] = 0;
        }
        return std.mem.strlen(nargv[idx]);
    } else {
        return -1;
    }
}

fn os_getenv(env: u32, adr: u32, siz: u32) u32 {
    mem_check_range(adr, siz, "OS.Getenv");
    const p = std.os.getenv(mem + env);
    if ((p != null) and siz > 0) {
        std.mem.copy(mem + adr, p, siz - 1);
        mem[adr + siz - 1] = 0;
        return 1;
    }
    return 0;
}

fn files_get_name(name: []u8, name_adr: u32) bool {
    name = "Hey!";
    return name_adr == 0;
}

fn norebo_trap(trap: u32, name_adr: u32, pos: u32) u32 {
    const message = switch (trap) {
        1 => "array index out of range",
        2 => "type guard failure",
        3 => "array or string copy overflow",
        4 => "access via NIL pointer",
        5 => "illegal procedure call",
        6 => "integer division by zero",
        7 => "assertion violated",
        else => std.fmt("unknown trap %d\n", trap),
    };
    var name: [NameLength]u8 = undefined;
    if (!files_get_name(name, name_adr)) {
        name = "(unknown)";
    }
    std.io.fprintf(std.io.getStdErr(), "%s at %s pos %d\n", message, name, pos);
    std.os.exit(100 + trap);
    return 0;
}

const SysreqFn = fn (u32, u32, u32) u32;

var sysreq_table: SysreqFn[40] = undefined;

fn init_sysreq() void {
    sysreq_table[0] = norebo_halt;
    sysreq_table[1] = norebo_trap;
    // etc...
}

const SysreqCnt = sysreq_table.len();

fn sysreq_exec(n: u32) u32 {
    if (n >= SysreqCnt || !sysreq_table[n]) {
        std.fprintf(std.io.getStdErr(), "Unimplemented sysreq %d\n", n);
        std.os.exit(1);
    }
    return sysreq_table[n](sysarg[0], sysarg[1], sysarg[2]);
}

fn risc_time() u32 {
    var tv: std.Timeval = undefined;
    std.os.gettimeofday(&tv, null);
    return (tv.tv_sec * 1000 + tv.tv_usec / 1000);
}

fn risc_leds(n: u32) void {
    var buf: [20]u8 = undefined;
    var i = 0;
    while (i < 8) {
        buf[14 - i] = if ((n & (1 << i)) != 0) ('0' + i) else '-';
        i = i + 1;
    }
    std.io.fputs(buf, std.io.getStdErr());
}

fn path_fopen(pathEnv: []u8, filename: []u8, mode: []u8) std.fs.File {
    const path = std.fmt("%s/%s", pathEnv, filename);
    return std.fs.fopen(path, mode);
}
// Boot

fn load_inner_core() u32 {
    var f = std.fs.fopen(InnerCore, "rb");
    if (!f) {
        f = path_fopen(std.os.getenv(PathEnv), InnerCore, "rb");
    }
    if (!f) {
        std.fprintf(std.io.getStdErr(), "Can't load %s", InnerCore);
        return -1;
    }

    var siz: u32 = undefined;
    if (!read_uint32(&siz, f)) {
        return -1;
    }
    while (siz != 0) {
        var adr: u32 = undefined;
        if (!read_uint32(&adr, f)) {
            return -1;
        }
        mem_check_range(adr, siz, InnerCore);
        const r = std.fs.fread(mem + adr, 1, siz, f);
        if (r != siz) {
            return -1;
        }
        if (!read_uint32(&siz, f)) {
            return -1;
        }
    }
    std.fs.fclose(f);
    return 0;
}

fn main(argc: i32, argv: [*]u8) i32 {
    nargc = argc - 1;
    nargv = argv[1..];

    load_inner_core();
    mem_write_word(12, MemBytes);
    mem_write_word(24, StackOrg);
    PC = 0;
    R[12] = 0x20;
    R[14] = StackOrg;
    risc_run();

    return 0;
}
