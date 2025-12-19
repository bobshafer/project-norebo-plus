const std = @import("std");

const PathEnv = "NOREBO_PATH";
const InnerCore = "InnerCore";

const MemBytes = 8 * 1024 * 1024;
const StackOrg: u32 = 0x80000;
const MaxFiles = 500;
const NameLength = 32;

const Pbit: u32 = 0x8000_0000;
const Qbit: u32 = 0x4000_0000;
const Ubit: u32 = 0x2000_0000;
const Vbit: u32 = 0x1000_0000;

const Op = enum(u4) {
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

const FileEntry = struct {
    file: ?std.fs.File = null,
    name: [NameLength]u8 = [_]u8{0} ** NameLength,
    registered: bool = false,
};

var mem: [MemBytes]u8 = [_]u8{0} ** MemBytes;
var sysarg: [3]u32 = .{ 0, 0, 0 };
var sysres: u32 = 0;
var nargc: u32 = 0;
var nargv: []const [:0]u8 = &.{};
var files: [MaxFiles]FileEntry = [_]FileEntry{.{}} ** MaxFiles;

var pc: u32 = 0;
var r: [16]u32 = [_]u32{0} ** 16;
var h: u32 = 0;
var z: bool = false;
var n: bool = false;
var c: bool = false;
var v: bool = false;

var dir_iter: ?std.fs.Dir.Iterator = null;

fn setRegister(reg: u4, value: u32) void {
    r[reg] = value;
    z = (value == 0);
    n = (@as(i32, @bitCast(value)) < 0);
}

fn le32ToHost(ptr: []const u8) u32 {
    return @as(u32, ptr[0]) |
        (@as(u32, ptr[1]) << 8) |
        (@as(u32, ptr[2]) << 16) |
        (@as(u32, ptr[3]) << 24);
}

fn memCheckRange(adr: u32, siz: u32, proc: []const u8) void {
    const mem_bytes: u32 = MemBytes;
    if (adr >= mem_bytes or mem_bytes - adr < siz) {
        std.debug.print("{s}: Memory access out of bounds\n", .{proc});
        std.process.exit(1);
    }
}

fn memReadWord(adr: u32) u32 {
    if (adr >= MemBytes - 3) {
        std.debug.print("Memory read out of bounds (address {x})\n", .{adr});
        std.process.exit(1);
    }
    return le32ToHost(mem[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + 4))]);
}

fn memReadByte(adr: u32) u8 {
    if (adr >= MemBytes) {
        std.debug.print("Memory read out of bounds (address {x})\n", .{adr});
        std.process.exit(1);
    }
    return mem[@as(usize, @intCast(adr))];
}

fn memWriteWord(adr: u32, val: u32) void {
    if (adr >= MemBytes - 3) {
        std.debug.print("Memory write out of bounds (address {x})\n", .{adr});
        std.process.exit(1);
    }
    const i: usize = @intCast(adr);
    mem[i] = @as(u8, @truncate(val));
    mem[i + 1] = @as(u8, @truncate(val >> 8));
    mem[i + 2] = @as(u8, @truncate(val >> 16));
    mem[i + 3] = @as(u8, @truncate(val >> 24));
}

fn memWriteByte(adr: u32, val: u8) void {
    if (adr >= MemBytes) {
        std.debug.print("Memory write out of bounds (address {x})\n", .{adr});
        std.process.exit(1);
    }
    mem[@as(usize, @intCast(adr))] = val;
}

fn ioReadWord(adr: u32) u32 {
    const slot = @as(i32, @bitCast(adr));
    switch (@divTrunc(-slot, 4)) {
        64 / 4 => return riscTime(),
        56 / 4 => {
            var buf: [1]u8 = undefined;
            const bytes_read = std.fs.File.stdin().read(&buf) catch 0;
            return if (bytes_read == 1) @as(u32, buf[0]) else 0;
        },
        52 / 4 => return 3,
        16 / 4 => return sysarg[2],
        12 / 4 => return sysarg[1],
        8 / 4 => return sysarg[0],
        4 / 4 => return sysres,
        else => {
            std.debug.print("Unimplemented read of I/O address {d}\n", .{adr});
            std.process.exit(1);
        },
    }
}

fn ioWriteWord(adr: u32, val: u32) void {
    const slot = @as(i32, @bitCast(adr));
    switch (@divTrunc(-slot, 4)) {
        60 / 4 => riscLeds(val),
        56 / 4 => {
            const out_byte: [1]u8 = .{@as(u8, @truncate(val))};
            _ = std.fs.File.stdout().writeAll(&out_byte) catch {};
        },
        16 / 4 => sysarg[2] = val,
        12 / 4 => sysarg[1] = val,
        8 / 4 => sysarg[0] = val,
        4 / 4 => sysres = sysreqExec(val),
        else => {
            std.debug.print("Unimplemented write of I/O address {d}\n", .{adr});
            std.process.exit(1);
        },
    }
}

fn cpuReadProgram(adr: u32) u32 {
    return memReadWord(adr * 4);
}

fn cpuReadWord(adr: u32) u32 {
    return if (@as(i32, @bitCast(adr)) >= 0) memReadWord(adr) else ioReadWord(adr);
}

fn cpuReadByte(adr: u32) u32 {
    return if (@as(i32, @bitCast(adr)) >= 0) @as(u32, memReadByte(adr)) else ioReadWord(adr);
}

fn cpuWriteWord(adr: u32, val: u32) void {
    if (@as(i32, @bitCast(adr)) >= 0) memWriteWord(adr, val) else ioWriteWord(adr, val);
}

fn cpuWriteByte(adr: u32, val: u8) void {
    if (@as(i32, @bitCast(adr)) >= 0) memWriteByte(adr, val) else ioWriteWord(adr, val);
}

fn readUint32(f: *std.fs.File) ?u32 {
    var buf: [4]u8 = undefined;
    if (f.readAll(&buf) catch 0 != 4) return null;
    return le32ToHost(&buf);
}

fn fpAdd(x: u32, y: u32, u: bool, vflag: bool) u32 {
    const xs = (x & 0x8000_0000) != 0;
    var xe: u32 = undefined;
    var x0: i32 = undefined;
    if (!u) {
        xe = (x >> 23) & 0xFF;
        const xm = ((x & 0x7FFFFF) << 1) | 0x1000000;
        x0 = if (xs) -@as(i32, @bitCast(xm)) else @as(i32, @bitCast(xm));
    } else {
        xe = 150;
        x0 = @as(i32, @bitCast(x & 0x00FF_FFFF)) << 8 >> 7;
    }

    const ys = (y & 0x8000_0000) != 0;
    const ye = (y >> 23) & 0xFF;
    var ym = (y & 0x7FFFFF) << 1;
    if (!u and !vflag) ym |= 0x1000000;
    const y0: i32 = if (ys) -@as(i32, @bitCast(ym)) else @as(i32, @bitCast(ym));

    var e0: u32 = undefined;
    var x3: i32 = undefined;
    var y3: i32 = undefined;
    if (ye > xe) {
        const shift = ye - xe;
        e0 = ye;
        x3 = if (shift > 31) x0 >> 31 else x0 >> @as(u5, @intCast(shift));
        y3 = y0;
    } else {
        const shift = xe - ye;
        e0 = xe;
        x3 = x0;
        y3 = if (shift > 31) y0 >> 31 else y0 >> @as(u5, @intCast(shift));
    }

    const sum = ((@as(u32, @intFromBool(xs)) << 26) | (@as(u32, @intFromBool(xs)) << 25) | (@as(u32, @bitCast(x3)) & 0x01FF_FFFF)) +%
        ((@as(u32, @intFromBool(ys)) << 26) | (@as(u32, @intFromBool(ys)) << 25) | (@as(u32, @bitCast(y3)) & 0x01FF_FFFF));

    var s = if ((sum & (1 << 26)) != 0) (0 -% sum) else sum;
    s +%= 1;
    s &= 0x07FF_FFFF;

    var e1 = e0 +% 1;
    var t3 = s >> 1;
    if ((s & 0x03FF_FFFC) != 0) {
        while ((t3 & (1 << 24)) == 0) {
            t3 <<= 1;
            e1 -%= 1;
        }
    } else {
        t3 <<= 24;
        e1 -%= 24;
    }

    if (vflag) {
        return @as(u32, @bitCast(@as(i32, @bitCast(sum << 5)) >> 6));
    } else if ((x & 0x7FFF_FFFF) == 0) {
        return if (!u) y else 0;
    } else if ((y & 0x7FFF_FFFF) == 0) {
        return x;
    } else if ((t3 & 0x01FF_FFFF) == 0 or (e1 & 0x100) != 0) {
        return 0;
    } else {
        return ((sum & 0x0400_0000) << 5) | (e1 << 23) | ((t3 >> 1) & 0x7FFFFF);
    }
}

fn fpMul(x: u32, y: u32) u32 {
    const sign = (x ^ y) & 0x8000_0000;
    const xe = (x >> 23) & 0xFF;
    const ye = (y >> 23) & 0xFF;

    const xm = (x & 0x7FFFFF) | 0x800000;
    const ym = (y & 0x7FFFFF) | 0x800000;
    const m = @as(u64, xm) * @as(u64, ym);

    var e1 = (xe +% ye) -% 127;
    var z0: u32 = undefined;
    if ((m & (1 << 47)) != 0) {
        e1 +%= 1;
        z0 = @as(u32, @intCast((m >> 23) + 1)) & 0x00FF_FFFF;
    } else {
        z0 = @as(u32, @intCast((m >> 22) + 1)) & 0x00FF_FFFF;
    }

    if (xe == 0 or ye == 0) {
        return 0;
    } else if ((e1 & 0x100) == 0) {
        return sign | ((e1 & 0xFF) << 23) | (z0 >> 1);
    } else if ((e1 & 0x80) == 0) {
        return sign | (0xFF << 23) | (z0 >> 1);
    } else {
        return 0;
    }
}

fn fpDiv(x: u32, y: u32) u32 {
    const sign = (x ^ y) & 0x8000_0000;
    const xe = (x >> 23) & 0xFF;
    const ye = (y >> 23) & 0xFF;

    const xm = (x & 0x7FFFFF) | 0x800000;
    const ym = (y & 0x7FFFFF) | 0x800000;
    const q1: u32 = @intCast((@as(u64, xm) << 25) / @as(u64, ym));

    var e1 = (xe -% ye) +% 126;
    var q2: u32 = undefined;
    if ((q1 & (1 << 25)) != 0) {
        e1 +%= 1;
        q2 = (q1 >> 1) & 0x00FF_FFFF;
    } else {
        q2 = q1 & 0x00FF_FFFF;
    }
    const q3 = q2 +% 1;

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

const Idiv = struct { quot: u32, rem: u32 };

fn idiv(x: u32, y: u32, signed_div: bool) Idiv {
    const sign = (@as(i32, @bitCast(x)) < 0) and signed_div;
    const x0: u32 = if (sign) (0 -% x) else x;

    var rq: u64 = x0;
    var s: u32 = 0;
    while (s < 32) : (s += 1) {
        const w0 = @as(u32, @intCast(rq >> 31));
        const w1 = w0 - y;
        if (@as(i32, @bitCast(w1)) < 0) {
            rq = (@as(u64, w0) << 32) | ((rq & 0x7FFF_FFFF) << 1);
        } else {
            rq = (@as(u64, w1) << 32) | ((rq & 0x7FFF_FFFF) << 1) | 1;
        }
    }

    var d = Idiv{ .quot = @as(u32, @intCast(rq)), .rem = @as(u32, @intCast(rq >> 32)) };
    if (sign) {
        d.quot = 0 -% d.quot;
        if (d.rem != 0) {
            d.quot -%= 1;
            d.rem = y - d.rem;
        }
    }
    return d;
}

fn riscRun() void {
    while (true) {
        const ir = cpuReadProgram(pc);
        pc +%= 1;

        if ((ir & Pbit) == 0) {
            const a: u4 = @intCast((ir >> 24) & 0xF);
            const b: u4 = @intCast((ir >> 20) & 0xF);
            const op: Op = @enumFromInt(@as(u4, @intCast((ir >> 16) & 0xF)));
            const im: u32 = ir & 0xFFFF;
            const c_reg: u4 = @intCast(ir & 0xF);

            const b_val = r[b];
            const c_val: u32 = if ((ir & Qbit) == 0)
                r[c_reg]
            else if ((ir & Vbit) == 0)
                im
            else
                0xFFFF_0000 | im;

            var a_val: u32 = 0;
            switch (op) {
                .MOV => {
                    if ((ir & Ubit) == 0) {
                        a_val = c_val;
                    } else if ((ir & Qbit) != 0) {
                        a_val = c_val << 16;
                    } else if ((ir & Vbit) != 0) {
                        a_val = 0xD0 |
                            (@as(u32, @intFromBool(n)) << 31) |
                            (@as(u32, @intFromBool(z)) << 30) |
                            (@as(u32, @intFromBool(c)) << 29) |
                            (@as(u32, @intFromBool(v)) << 28);
                    } else {
                        a_val = h;
                    }
                },
                .LSL => a_val = b_val << @as(u5, @intCast(c_val & 31)),
                .ASR => a_val = @as(u32, @bitCast(@as(i32, @bitCast(b_val)) >> @as(u5, @intCast(c_val & 31)))),
                .ROR => {
                    const sh: u5 = @as(u5, @intCast(c_val & 31));
                    const rsh: u5 = @as(u5, @intCast((32 - @as(u32, sh)) & 31));
                    a_val = (b_val >> sh) | (b_val << rsh);
                },
                .AND => a_val = b_val & c_val,
                .ANN => a_val = b_val & ~c_val,
                .IOR => a_val = b_val | c_val,
                .XOR => a_val = b_val ^ c_val,
                .ADD => {
                    var tmp = b_val +% c_val;
                    if ((ir & Ubit) != 0) tmp +%= @as(u32, @intFromBool(c));
                    c = tmp < b_val;
                    v = (((tmp ^ c_val) & (tmp ^ b_val)) >> 31) != 0;
                    a_val = tmp;
                },
                .SUB => {
                    var tmp = b_val -% c_val;
                    if ((ir & Ubit) != 0) tmp -%= @as(u32, @intFromBool(c));
                    c = tmp > b_val;
                    v = (((b_val ^ c_val) & (tmp ^ b_val)) >> 31) != 0;
                    a_val = tmp;
                },
                .MUL => {
                    if ((ir & Ubit) == 0) {
                        const tmp = @as(i64, @intCast(@as(i32, @bitCast(b_val)))) *
                            @as(i64, @intCast(@as(i32, @bitCast(c_val))));
                        a_val = @as(u32, @bitCast(@as(i32, @intCast(tmp))));
                        h = @as(u32, @bitCast(@as(i32, @intCast(tmp >> 32))));
                    } else {
                        const tmp: u64 = @as(u64, b_val) * @as(u64, c_val);
                        a_val = @as(u32, @intCast(tmp));
                        h = @as(u32, @intCast(tmp >> 32));
                    }
                },
                .DIV => {
                    if (@as(i32, @bitCast(c_val)) > 0) {
                        if ((ir & Ubit) == 0) {
                            a_val = @as(u32, @bitCast(@divTrunc(@as(i32, @bitCast(b_val)), @as(i32, @bitCast(c_val)))));
                            h = @as(u32, @bitCast(@rem(@as(i32, @bitCast(b_val)), @as(i32, @bitCast(c_val)))));
                            if (@as(i32, @bitCast(h)) < 0) {
                                a_val -%= 1;
                                h +%= c_val;
                            }
                        } else {
                            a_val = b_val / c_val;
                            h = b_val % c_val;
                        }
                    } else {
                        const q = idiv(b_val, c_val, (ir & Ubit) != 0);
                        a_val = q.quot;
                        h = q.rem;
                    }
                },
                .FAD => a_val = fpAdd(b_val, c_val, (ir & Ubit) != 0, (ir & Vbit) != 0),
                .FSB => a_val = fpAdd(b_val, c_val ^ 0x8000_0000, (ir & Ubit) != 0, (ir & Vbit) != 0),
                .FML => a_val = fpMul(b_val, c_val),
                .FDV => a_val = fpDiv(b_val, c_val),
            }
            setRegister(a, a_val);
        } else if ((ir & Qbit) == 0) {
            const a: u4 = @intCast((ir >> 24) & 0xF);
            const b: u4 = @intCast((ir >> 20) & 0xF);
            var off: i32 = @bitCast(ir & 0x000F_FFFF);
            off = (off ^ 0x0008_0000) - 0x0008_0000;

            const address = r[b] +% @as(u32, @bitCast(off));
            if ((ir & Ubit) == 0) {
                const a_val = if ((ir & Vbit) == 0) cpuReadWord(address) else cpuReadByte(address);
                setRegister(a, a_val);
            } else {
                if ((ir & Vbit) == 0) {
                    cpuWriteWord(address, r[a]);
                } else {
                    cpuWriteByte(address, @as(u8, @truncate(r[a])));
                }
            }
        } else {
            var t = ((ir >> 27) & 1) != 0;
            switch ((ir >> 24) & 7) {
                0 => t = t ^ n,
                1 => t = t ^ z,
                2 => t = t ^ c,
                3 => t = t ^ v,
                4 => t = t ^ (c or z),
                5 => t = t ^ (n ^ v),
                6 => t = t ^ ((n ^ v) or z),
                7 => t = t ^ true,
                else => unreachable,
            }
            if (t) {
                if ((ir & Vbit) != 0) {
                    setRegister(15, pc * 4);
                }
                if ((ir & Ubit) == 0) {
                    const c_reg: u4 = @intCast(ir & 0xF);
                    pc = r[c_reg] / 4;
                } else {
                    var off: i32 = @bitCast(ir & 0x00FF_FFFF);
                    off = (off ^ 0x0080_0000) - 0x0080_0000;
                    pc = pc +% @as(u32, @bitCast(off));
                }
            }
        }
    }
}

fn timeToOberon(t: i64) u32 {
    const epoch_seconds = std.time.epoch.EpochSeconds{ .secs = @as(u64, @intCast(t)) };
    const year_day = epoch_seconds.getEpochDay().calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    const day_seconds = epoch_seconds.getDaySeconds();

    const year = @as(u32, @intCast(year_day.year % 100));
    const month = @as(u32, @intCast(month_day.month.numeric()));
    const day = @as(u32, @intCast(month_day.day_index)) + 1;
    const hour = @as(u32, @intCast(day_seconds.getHoursIntoDay()));
    const minute = @as(u32, @intCast(day_seconds.getMinutesIntoHour()));
    const second = @as(u32, @intCast(day_seconds.getSecondsIntoMinute()));

    return (year * 0x4000000) | (month * 0x400000) | (day * 0x20000) |
        (hour * 0x1000) | (minute * 0x40) | second;
}

fn riscTime() u32 {
    const ms = std.time.milliTimestamp();
    return @as(u32, @intCast(ms));
}

fn riscLeds(nval: u32) void {
    var buf = "[LEDs: 76543210]\n".*;
    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const idx: usize = 14 - @as(usize, @intCast(i));
        const shift: u5 = @as(u5, @intCast(i));
        const mask: u32 = @as(u32, 1) << shift;
        buf[idx] = if ((nval & mask) != 0) @as(u8, '0' + @as(u8, @intCast(i))) else '-';
    }
    _ = std.fs.File.stderr().writeAll(&buf) catch {};
}

fn filesCheckName(name: []const u8) bool {
    var i: usize = 0;
    while (i < NameLength) : (i += 1) {
        const ch = name[i];
        if (ch == 0) {
            return true;
        } else if (!((ch >= 'A' and ch <= 'Z') or (ch >= 'a' and ch <= 'z') or
            (i > 0 and (ch == '.' or (ch >= '0' and ch <= '9')))))
        {
            return false;
        }
    }
    return false;
}

fn filesGetName(name: *[NameLength]u8, adr: u32) bool {
    memCheckRange(adr, NameLength, "Files.GetName");
    const start: usize = @intCast(adr);
    std.mem.copyForwards(u8, name[0..], mem[start .. start + NameLength]);
    return filesCheckName(name);
}

fn filesAllocate(name: []const u8, registered: bool) u32 {
    var hidx: usize = 0;
    while (hidx < MaxFiles) : (hidx += 1) {
        if (files[hidx].file == null) {
            @memset(files[hidx].name[0..], 0);
            std.mem.copyForwards(u8, files[hidx].name[0..name.len], name);
            files[hidx].registered = registered;
            return @as(u32, @intCast(hidx));
        }
    }
    std.debug.print("Files.Allocate: Too many open files\n", .{});
    std.process.exit(1);
}

fn filesCheckHandle(hidx: u32, proc: []const u8) void {
    if (hidx >= MaxFiles or files[@as(usize, @intCast(hidx))].file == null) {
        std.debug.print("{s}: Invalid file handle\n", .{proc});
        std.process.exit(1);
    }
}

fn openFromPath(path_env: []const u8, filename: []const u8, write: bool) ?std.fs.File {
    const sep: u8 = if (std.mem.indexOfScalar(u8, path_env, ';') != null) ';' else ':';
    var it = std.mem.splitScalar(u8, path_env, sep);
    const allocator = std.heap.page_allocator;

    while (it.next()) |part| {
        if (part.len == 0) {
            return std.fs.cwd().openFile(filename, .{ .mode = if (write) .read_write else .read_only }) catch null;
        }
        const path = std.fs.path.join(allocator, &.{ part, filename }) catch continue;
        defer allocator.free(path);
        const f = std.fs.cwd().openFile(path, .{ .mode = if (write) .read_write else .read_only }) catch null;
        if (f) |file| return file;
    }
    return null;
}

fn filesNew(name_adr: u32, _: u32, _: u32) u32 {
    var name_buf: [NameLength]u8 = undefined;
    if (!filesGetName(&name_buf, name_adr)) return 0xFFFF_FFFF;
    const name = std.mem.sliceTo(&name_buf, 0);
    const hidx = filesAllocate(name, false);

    var tmp_name_buf: [64]u8 = undefined;
    const tmp_name = std.fmt.bufPrint(&tmp_name_buf, ".norebo_tmp_{d}", .{hidx}) catch return 0xFFFF_FFFF;
    const file = std.fs.cwd().createFile(tmp_name, .{ .truncate = true, .read = true }) catch {
        std.debug.print("Files.New: {s}\n", .{name});
        std.process.exit(1);
    };

    files[@as(usize, @intCast(hidx))].file = file;
    return hidx;
}

fn filesOld(name_adr: u32, _: u32, _: u32) u32 {
    var name_buf: [NameLength]u8 = undefined;
    if (!filesGetName(&name_buf, name_adr)) return 0xFFFF_FFFF;
    const name = std.mem.sliceTo(&name_buf, 0);
    const hidx = filesAllocate(name, true);

    var file = std.fs.cwd().openFile(name, .{ .mode = .read_write }) catch null;
    if (file == null) {
        if (std.process.getEnvVarOwned(std.heap.page_allocator, PathEnv) catch null) |path_env| {
            defer std.heap.page_allocator.free(path_env);
            file = openFromPath(path_env, name, false);
        }
    }
    if (file == null) {
        files[@as(usize, @intCast(hidx))] = .{};
        return 0xFFFF_FFFF;
    }
    files[@as(usize, @intCast(hidx))].file = file;
    return hidx;
}

fn filesRegister(hidx: u32, _: u32, _: u32) u32 {
    filesCheckHandle(hidx, "Files.Register");
    const entry = &files[@as(usize, @intCast(hidx))];
    if (!entry.registered and entry.name[0] != 0) {
        const name = std.mem.sliceTo(&entry.name, 0);
        const old_file = entry.file.?;
        const new_file = std.fs.cwd().createFile(name, .{ .truncate = true, .read = true }) catch {
            std.debug.print("Can't create file {s}\n", .{name});
            std.process.exit(1);
        };

        entry.file = new_file;
        _ = old_file.seekTo(0) catch {};
        var buf: [8192]u8 = undefined;
        while (true) {
            const in_n = old_file.read(&buf) catch 0;
            if (in_n == 0) break;
            const out_n = new_file.write(buf[0..in_n]) catch 0;
            if (out_n != in_n) {
                std.debug.print("Can't write file {s}\n", .{name});
                std.process.exit(1);
            }
        }
        old_file.close();
        _ = new_file.sync() catch {};
        entry.registered = true;
    }
    return 0;
}

fn filesClose(hidx: u32, _: u32, _: u32) u32 {
    filesCheckHandle(hidx, "Files.Close");
    const entry = &files[@as(usize, @intCast(hidx))];
    entry.file.?.close();
    entry.* = .{};
    return 0;
}

fn filesSeek(hidx: u32, pos: u32, _: u32) u32 {
    filesCheckHandle(hidx, "Files.Seek");
    if (files[@as(usize, @intCast(hidx))].file.?.seekTo(pos)) |_| {
        return 0;
    } else |_| {
        return 1;
    }
}

fn filesTell(hidx: u32, _: u32, _: u32) u32 {
    filesCheckHandle(hidx, "Files.Tell");
    return @as(u32, @intCast(files[@as(usize, @intCast(hidx))].file.?.getPos() catch 0));
}

fn filesRead(hidx: u32, adr: u32, siz: u32) u32 {
    filesCheckHandle(hidx, "Files.Read");
    memCheckRange(adr, siz, "Files.Read");
    const f = files[@as(usize, @intCast(hidx))].file.?;
    const slice = mem[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + siz))];
    const rlen = f.read(slice) catch 0;
    if (rlen < slice.len) {
        @memset(slice[rlen..], 0);
    }
    return @as(u32, @intCast(rlen));
}

fn filesWrite(hidx: u32, adr: u32, siz: u32) u32 {
    filesCheckHandle(hidx, "Files.Write");
    memCheckRange(adr, siz, "Files.Write");
    const f = files[@as(usize, @intCast(hidx))].file.?;
    const slice = mem[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + siz))];
    const wlen = f.write(slice) catch 0;
    return @as(u32, @intCast(wlen));
}

fn filesLength(hidx: u32, _: u32, _: u32) u32 {
    filesCheckHandle(hidx, "Files.Length");
    const f = files[@as(usize, @intCast(hidx))].file.?;
    _ = f.sync() catch {};
    const stat = f.stat() catch {
        std.debug.print("Files.Length\n", .{});
        std.process.exit(1);
    };
    return @as(u32, @intCast(stat.size));
}

fn filesDate(hidx: u32, _: u32, _: u32) u32 {
    filesCheckHandle(hidx, "Files.Date");
    const f = files[@as(usize, @intCast(hidx))].file.?;
    _ = f.sync() catch {};
    if (files[@as(usize, @intCast(hidx))].registered) {
        const stat = f.stat() catch {
            std.debug.print("Files.Date\n", .{});
            std.process.exit(1);
        };
        return timeToOberon(@as(i64, @intCast(stat.mtime)));
    }
    return timeToOberon(@as(i64, @intCast(std.time.timestamp())));
}

fn filesDelete(name_adr: u32, _: u32, _: u32) u32 {
    var name_buf: [NameLength]u8 = undefined;
    if (!filesGetName(&name_buf, name_adr) or name_buf[0] == 0) return 0xFFFF_FFFF;
    const name = std.mem.sliceTo(&name_buf, 0);
    if (std.fs.cwd().deleteFile(name)) |_| {} else |_| return 0xFFFF_FFFF;
    return 0;
}

fn filesPurge(_: u32, _: u32, _: u32) u32 {
    std.debug.print("Files.Purge not implemented\n", .{});
    std.process.exit(1);
}

fn filesRename(old_adr: u32, new_adr: u32, _: u32) u32 {
    var old_buf: [NameLength]u8 = undefined;
    var new_buf: [NameLength]u8 = undefined;
    if (!filesGetName(&old_buf, old_adr) or old_buf[0] == 0) return 0xFFFF_FFFF;
    if (!filesGetName(&new_buf, new_adr) or new_buf[0] == 0) return 0xFFFF_FFFF;
    const old_name = std.mem.sliceTo(&old_buf, 0);
    const new_name = std.mem.sliceTo(&new_buf, 0);
    if (std.fs.cwd().rename(old_name, new_name)) |_| {} else |_| return 0xFFFF_FFFF;
    return 0;
}

fn filedirEnumerateBegin(_: u32, _: u32, _: u32) u32 {
    dir_iter = std.fs.cwd().iterate();
    return 0;
}

fn filedirEnumerateNext(adr: u32, _: u32, _: u32) u32 {
    memCheckRange(adr, NameLength, "FileDir.EnumerateNext");
    if (dir_iter == null) {
        memWriteByte(adr, 0);
        return 0xFFFF_FFFF;
    }
    var it = dir_iter.?;
    while (it.next() catch null) |ent| {
        if (filesCheckName(ent.name)) {
            const dst = mem[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + NameLength))];
            @memset(dst, 0);
            std.mem.copyForwards(u8, dst[0..ent.name.len], ent.name);
            dir_iter = it;
            return 0;
        }
    }
    dir_iter = null;
    memWriteByte(adr, 0);
    return 0xFFFF_FFFF;
}

fn filedirEnumerateEnd(_: u32, _: u32, _: u32) u32 {
    dir_iter = null;
    return 0;
}

fn noreboHalt(ec: u32, _: u32, _: u32) u32 {
    std.process.exit(@as(u8, @intCast(ec)));
}

fn noreboArgc(_: u32, _: u32, _: u32) u32 {
    return nargc;
}

fn noreboArgv(idx: u32, adr: u32, siz: u32) u32 {
    memCheckRange(adr, siz, "Norebo.Argv");
    if (idx < nargc) {
        if (siz > 0) {
            const src = nargv[@as(usize, @intCast(idx))];
            const dst = mem[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + siz))];
            @memset(dst, 0);
            const len = @min(src.len, dst.len - 1);
            std.mem.copyForwards(u8, dst[0..len], src[0..len]);
        }
        return @as(u32, @intCast(nargv[@as(usize, @intCast(idx))].len));
    }
    return 0xFFFF_FFFF;
}

fn noreboSystem(adr: u32, len: u32, _: u32) u32 {
    if (len == 0) return 0;
    memCheckRange(adr, len, "Norebo.System");
    const allocator = std.heap.page_allocator;
    const slice = mem[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + len))];
    const buf = allocator.alloc(u8, slice.len + 1) catch {
        std.debug.print("Norebo.System: out of memory\n", .{});
        std.process.exit(1);
    };
    defer allocator.free(buf);
    std.mem.copyForwards(u8, buf[0..slice.len], slice);
    buf[slice.len] = 0;

    const argv = [_][]const u8{ "/bin/sh", "-c", buf[0..slice.len] };
    var child = std.process.Child.init(&argv, allocator);
    child.stdin_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;
    const term = child.spawnAndWait() catch return 0xFFFF_FFFF;
    return switch (term) {
        .Exited => |code| @as(u32, @intCast(code)),
        else => 0xFFFF_FFFF,
    };
}

fn osGetenv(env_adr: u32, adr: u32, siz: u32) u32 {
    memCheckRange(adr, siz, "OS.Getenv");
    var name_buf: [NameLength]u8 = undefined;
    memCheckRange(env_adr, NameLength, "OS.Getenv");
    std.mem.copyForwards(u8, name_buf[0..], mem[@as(usize, @intCast(env_adr)) .. @as(usize, @intCast(env_adr + NameLength))]);
    const name = std.mem.sliceTo(&name_buf, 0);
    const value = std.process.getEnvVarOwned(std.heap.page_allocator, name) catch null;
    if (value) |val| {
        defer std.heap.page_allocator.free(val);
        if (siz > 0) {
            const dst = mem[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + siz))];
            @memset(dst, 0);
            const len = @min(val.len, dst.len - 1);
            std.mem.copyForwards(u8, dst[0..len], val[0..len]);
        }
        return 1;
    }
    return 0;
}

fn noreboTrap(trap: u32, name_adr: u32, pos: u32) u32 {
    var message: []const u8 = "unknown trap";
    switch (trap) {
        1 => message = "array index out of range",
        2 => message = "type guard failure",
        3 => message = "array or string copy overflow",
        4 => message = "access via NIL pointer",
        5 => message = "illegal procedure call",
        6 => message = "integer division by zero",
        7 => message = "assertion violated",
        else => {},
    }
    var name_buf: [NameLength]u8 = undefined;
    const name_ok = filesGetName(&name_buf, name_adr);
    const name = if (name_ok) std.mem.sliceTo(&name_buf, 0) else "(unknown)";
    std.debug.print("{s} at {s} pos {d}\n", .{ message, name, pos });
    std.process.exit(100 + @as(u8, @intCast(trap)));
}

const SysreqFn = *const fn (u32, u32, u32) u32;

const sysreq_table = [_]?SysreqFn{
    null,
    noreboHalt,
    noreboArgc,
    noreboArgv,
    noreboTrap,
    null,
    null,
    null,
    null,
    null,
    null,
    filesNew,
    filesOld,
    filesRegister,
    filesClose,
    filesSeek,
    filesTell,
    filesRead,
    filesWrite,
    filesLength,
    filesDate,
    filesDelete,
    filesPurge,
    filesRename,
    null,
    null,
    null,
    null,
    null,
    null,
    null,
    filedirEnumerateBegin,
    filedirEnumerateNext,
    filedirEnumerateEnd,
    null,
    null,
    null,
    null,
    null,
    null,
    null,
    osGetenv,
    noreboSystem,
};

fn sysreqExec(nval: u32) u32 {
    if (nval >= sysreq_table.len or sysreq_table[nval] == null) {
        std.debug.print("Unimplemented sysreq {d}\n", .{nval});
        std.process.exit(1);
    }
    return sysreq_table[nval].?(sysarg[0], sysarg[1], sysarg[2]);
}

fn loadInnerCore() void {
    var file: ?std.fs.File = std.fs.cwd().openFile(InnerCore, .{ .mode = .read_only }) catch null;
    if (file == null) {
        if (std.process.getEnvVarOwned(std.heap.page_allocator, PathEnv) catch null) |path_env| {
            defer std.heap.page_allocator.free(path_env);
            file = openFromPath(path_env, InnerCore, false);
        }
    }
    if (file == null) {
        std.debug.print("Can't load {s}\n", .{InnerCore});
        std.process.exit(1);
    }
    var f = file.?;
    defer f.close();

    var siz = readUint32(&f) orelse unreachable;
    while (siz != 0) {
        const adr = readUint32(&f) orelse unreachable;
        memCheckRange(adr, siz, InnerCore);
        const slice = mem[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + siz))];
        if (f.readAll(slice) catch 0 != slice.len) {
            std.debug.print("Error while reading {s}\n", .{InnerCore});
            std.process.exit(1);
        }
        siz = readUint32(&f) orelse 0;
    }
}

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);
    if (args.len > 1) {
        nargc = @as(u32, @intCast(args.len - 1));
        nargv = args[1..];
    }

    loadInnerCore();
    memWriteWord(12, MemBytes);
    memWriteWord(24, StackOrg);
    pc = 0;
    r[12] = 0x20;
    r[14] = StackOrg;
    riscRun();
}
