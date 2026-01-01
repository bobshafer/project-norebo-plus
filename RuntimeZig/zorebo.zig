const std = @import("std");
const build_options = @import("build_options");

const PathEnv = "NOREBO_PATH";
const InnerCore = "InnerCore";

const MemBytes = 8 * 1024 * 1024;
const StackOrg: u32 = 0x80000;
const MaxFiles = 500;
const NameLength = 32;
const TempPrefix = ".norebo_tmp_";
const EnableChecks = build_options.enable_checks;

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

const Flags = packed struct {
    z: bool = false,
    n: bool = false,
    c: bool = false,
    v: bool = false,
};

const Vm = struct {
    pc: u32 = 0,
    r: [16]u32 = [_]u32{0} ** 16,
    h: u32 = 0,
    flags: Flags = .{},
};

const Memory = struct {
    data: [MemBytes]u8 = [_]u8{0} ** MemBytes,

    inline fn checkRange(self: *Memory, rt: *Runtime, adr: u32, siz: u32, proc: []const u8) void {
        _ = self;
        if (!EnableChecks) return;
        const mem_bytes: u32 = MemBytes;
        if (adr >= mem_bytes or mem_bytes - adr < siz) {
            std.debug.print("{s}: Memory access out of bounds\n", .{proc});
            noreboExit(rt, 1);
        }
    }

    inline fn readWord(self: *Memory, rt: *Runtime, adr: u32) u32 {
        if (EnableChecks) {
            if (adr >= MemBytes - 3) {
                std.debug.print("Memory read out of bounds (address {x})\n", .{adr});
                noreboExit(rt, 1);
            }
        }
        return le32ToHost(self.data[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + 4))]);
    }

    inline fn readByte(self: *Memory, rt: *Runtime, adr: u32) u8 {
        if (EnableChecks) {
            if (adr >= MemBytes) {
                std.debug.print("Memory read out of bounds (address {x})\n", .{adr});
                noreboExit(rt, 1);
            }
        }
        return self.data[@as(usize, @intCast(adr))];
    }

    inline fn writeWord(self: *Memory, rt: *Runtime, adr: u32, val: u32) void {
        if (EnableChecks) {
            if (adr >= MemBytes - 3) {
                std.debug.print("Memory write out of bounds (address {x})\n", .{adr});
                noreboExit(rt, 1);
            }
        }
        const i: usize = @intCast(adr);
        self.data[i] = @as(u8, @truncate(val));
        self.data[i + 1] = @as(u8, @truncate(val >> 8));
        self.data[i + 2] = @as(u8, @truncate(val >> 16));
        self.data[i + 3] = @as(u8, @truncate(val >> 24));
    }

    inline fn writeByte(self: *Memory, rt: *Runtime, adr: u32, val: u8) void {
        if (EnableChecks) {
            if (adr >= MemBytes) {
                std.debug.print("Memory write out of bounds (address {x})\n", .{adr});
                noreboExit(rt, 1);
            }
        }
        self.data[@as(usize, @intCast(adr))] = val;
    }
};

const FileEntry = struct {
    file: ?std.fs.File = null,
    name: [NameLength]u8 = [_]u8{0} ** NameLength,
    registered: bool = false,
};

const FileTable = struct {
    entries: [MaxFiles]FileEntry = [_]FileEntry{.{}} ** MaxFiles,
    dir_iter: ?std.fs.Dir.Iterator = null,

    fn checkName(name: []const u8) bool {
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

    fn getName(self: *FileTable, rt: *Runtime, name: *[NameLength]u8, adr: u32) bool {
        _ = self;
        rt.mem.checkRange(rt, adr, NameLength, "Files.GetName");
        const start: usize = @intCast(adr);
        std.mem.copyForwards(u8, name[0..], rt.mem.data[start .. start + NameLength]);
        return checkName(name);
    }

    fn allocate(self: *FileTable, rt: *Runtime, name: []const u8, registered: bool) u32 {
        var hidx: usize = 0;
        while (hidx < MaxFiles) : (hidx += 1) {
            if (self.entries[hidx].file == null) {
                @memset(self.entries[hidx].name[0..], 0);
                std.mem.copyForwards(u8, self.entries[hidx].name[0..name.len], name);
                self.entries[hidx].registered = registered;
                return @as(u32, @intCast(hidx));
            }
        }
        std.debug.print("Files.Allocate: Too many open files\n", .{});
        noreboExit(rt, 1);
    }

    fn checkHandle(self: *FileTable, rt: *Runtime, hidx: u32, proc: []const u8) void {
        if (hidx >= MaxFiles or self.entries[@as(usize, @intCast(hidx))].file == null) {
            std.debug.print("{s}: Invalid file handle\n", .{proc});
            noreboExit(rt, 1);
        }
    }

    fn new(self: *FileTable, rt: *Runtime, name_adr: u32, _: u32, _: u32) u32 {
        var name_buf: [NameLength]u8 = undefined;
        if (!self.getName(rt, &name_buf, name_adr)) return 0xFFFF_FFFF;
        const name = std.mem.sliceTo(&name_buf, 0);
        const hidx = self.allocate(rt, name, false);

        var tmp_name_buf: [64]u8 = undefined;
        const tmp_name = std.fmt.bufPrint(&tmp_name_buf, ".norebo_tmp_{d}", .{hidx}) catch return 0xFFFF_FFFF;
        const file = std.fs.cwd().createFile(tmp_name, .{ .truncate = true, .read = true }) catch {
            std.debug.print("Files.New: {s}\n", .{name});
            noreboExit(rt, 1);
        };

        self.entries[@as(usize, @intCast(hidx))].file = file;
        return hidx;
    }

    fn old(self: *FileTable, rt: *Runtime, name_adr: u32, _: u32, _: u32) u32 {
        var name_buf: [NameLength]u8 = undefined;
        if (!self.getName(rt, &name_buf, name_adr)) return 0xFFFF_FFFF;
        const name = std.mem.sliceTo(&name_buf, 0);
        const hidx = self.allocate(rt, name, true);

        var file = std.fs.cwd().openFile(name, .{ .mode = .read_write }) catch null;
        if (file == null) {
            if (std.process.getEnvVarOwned(std.heap.page_allocator, PathEnv) catch null) |path_env| {
                defer std.heap.page_allocator.free(path_env);
                file = openFromPath(path_env, name, false);
            }
        }
        if (file == null) {
            self.entries[@as(usize, @intCast(hidx))] = .{};
            return 0xFFFF_FFFF;
        }
        self.entries[@as(usize, @intCast(hidx))].file = file;
        return hidx;
    }

    fn register(self: *FileTable, rt: *Runtime, hidx: u32, _: u32, _: u32) u32 {
        self.checkHandle(rt, hidx, "Files.Register");
        const entry = &self.entries[@as(usize, @intCast(hidx))];
        if (!entry.registered and entry.name[0] != 0) {
            const name = std.mem.sliceTo(&entry.name, 0);
            const old_file = entry.file.?;
            const new_file = std.fs.cwd().createFile(name, .{ .truncate = true, .read = true }) catch {
                std.debug.print("Can't create file {s}\n", .{name});
                noreboExit(rt, 1);
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
                    noreboExit(rt, 1);
                }
            }
            old_file.close();
            cleanupTempForHandle(@as(usize, @intCast(hidx)));
            _ = new_file.sync() catch {};
            entry.registered = true;
        }
        return 0;
    }

    fn close(self: *FileTable, rt: *Runtime, hidx: u32, _: u32, _: u32) u32 {
        self.checkHandle(rt, hidx, "Files.Close");
        const entry = &self.entries[@as(usize, @intCast(hidx))];
        entry.file.?.close();
        if (!entry.registered) {
            cleanupTempForHandle(@as(usize, @intCast(hidx)));
        }
        entry.* = .{};
        return 0;
    }

    fn seek(self: *FileTable, rt: *Runtime, hidx: u32, pos: u32, _: u32) u32 {
        self.checkHandle(rt, hidx, "Files.Seek");
        if (self.entries[@as(usize, @intCast(hidx))].file.?.seekTo(pos)) |_| {
            return 0;
        } else |_| {
            return 1;
        }
    }

    fn tell(self: *FileTable, rt: *Runtime, hidx: u32, _: u32, _: u32) u32 {
        self.checkHandle(rt, hidx, "Files.Tell");
        return @as(u32, @intCast(self.entries[@as(usize, @intCast(hidx))].file.?.getPos() catch 0));
    }

    fn read(self: *FileTable, rt: *Runtime, hidx: u32, adr: u32, siz: u32) u32 {
        self.checkHandle(rt, hidx, "Files.Read");
        rt.mem.checkRange(rt, adr, siz, "Files.Read");
        const f = self.entries[@as(usize, @intCast(hidx))].file.?;
        const slice = rt.mem.data[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + siz))];
        const rlen = f.read(slice) catch 0;
        if (rlen < slice.len) {
            @memset(slice[rlen..], 0);
        }
        return @as(u32, @intCast(rlen));
    }

    fn write(self: *FileTable, rt: *Runtime, hidx: u32, adr: u32, siz: u32) u32 {
        self.checkHandle(rt, hidx, "Files.Write");
        rt.mem.checkRange(rt, adr, siz, "Files.Write");
        const f = self.entries[@as(usize, @intCast(hidx))].file.?;
        const slice = rt.mem.data[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + siz))];
        const wlen = f.write(slice) catch 0;
        return @as(u32, @intCast(wlen));
    }

    fn length(self: *FileTable, rt: *Runtime, hidx: u32, _: u32, _: u32) u32 {
        self.checkHandle(rt, hidx, "Files.Length");
        const f = self.entries[@as(usize, @intCast(hidx))].file.?;
        _ = f.sync() catch {};
        const stat = f.stat() catch {
            std.debug.print("Files.Length\n", .{});
            noreboExit(rt, 1);
        };
        return @as(u32, @intCast(stat.size));
    }

    fn date(self: *FileTable, rt: *Runtime, hidx: u32, _: u32, _: u32) u32 {
        self.checkHandle(rt, hidx, "Files.Date");
        const f = self.entries[@as(usize, @intCast(hidx))].file.?;
        _ = f.sync() catch {};
        if (self.entries[@as(usize, @intCast(hidx))].registered) {
            const stat = f.stat() catch {
                std.debug.print("Files.Date\n", .{});
                noreboExit(rt, 1);
            };
            return timeToOberon(@as(i64, @intCast(stat.mtime)));
        }
        return timeToOberon(@as(i64, @intCast(std.time.timestamp())));
    }

    fn delete(self: *FileTable, rt: *Runtime, name_adr: u32, _: u32, _: u32) u32 {
        var name_buf: [NameLength]u8 = undefined;
        if (!self.getName(rt, &name_buf, name_adr) or name_buf[0] == 0) return 0xFFFF_FFFF;
        const name = std.mem.sliceTo(&name_buf, 0);
        if (std.fs.cwd().deleteFile(name)) |_| {} else |_| return 0xFFFF_FFFF;
        return 0;
    }

    fn purge(self: *FileTable, rt: *Runtime, _: u32, _: u32, _: u32) u32 {
        _ = self;
        std.debug.print("Files.Purge not implemented\n", .{});
        noreboExit(rt, 1);
    }

    fn rename(self: *FileTable, rt: *Runtime, old_adr: u32, new_adr: u32, _: u32) u32 {
        var old_buf: [NameLength]u8 = undefined;
        var new_buf: [NameLength]u8 = undefined;
        if (!self.getName(rt, &old_buf, old_adr) or old_buf[0] == 0) return 0xFFFF_FFFF;
        if (!self.getName(rt, &new_buf, new_adr) or new_buf[0] == 0) return 0xFFFF_FFFF;
        const old_name = std.mem.sliceTo(&old_buf, 0);
        const new_name = std.mem.sliceTo(&new_buf, 0);
        if (std.fs.cwd().rename(old_name, new_name)) |_| {} else |_| return 0xFFFF_FFFF;
        return 0;
    }

    fn enumerateBegin(self: *FileTable, _: *Runtime, _: u32, _: u32, _: u32) u32 {
        self.dir_iter = std.fs.cwd().iterate();
        return 0;
    }

    fn enumerateNext(self: *FileTable, rt: *Runtime, adr: u32, _: u32, _: u32) u32 {
        rt.mem.checkRange(rt, adr, NameLength, "FileDir.EnumerateNext");
        if (self.dir_iter == null) {
            rt.mem.writeByte(rt, adr, 0);
            return 0xFFFF_FFFF;
        }
        var it = self.dir_iter.?;
        while (it.next() catch null) |ent| {
            if (checkName(ent.name)) {
                const dst = rt.mem.data[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + NameLength))];
                @memset(dst, 0);
                std.mem.copyForwards(u8, dst[0..ent.name.len], ent.name);
                self.dir_iter = it;
                return 0;
            }
        }
        self.dir_iter = null;
        rt.mem.writeByte(rt, adr, 0);
        return 0xFFFF_FFFF;
    }

    fn enumerateEnd(self: *FileTable, _: *Runtime, _: u32, _: u32, _: u32) u32 {
        self.dir_iter = null;
        return 0;
    }
};

const Io = struct {
    inline fn readWord(self: *Io, rt: *Runtime, adr: u32) u32 {
        _ = self;
        const slot = @as(i32, @bitCast(adr));
        switch (@divTrunc(-slot, 4)) {
            64 / 4 => return riscTime(),
            56 / 4 => {
                var buf: [1]u8 = undefined;
                const bytes_read = std.fs.File.stdin().read(&buf) catch 0;
                return if (bytes_read == 1) @as(u32, buf[0]) else 0;
            },
            52 / 4 => return 3,
            16 / 4 => return rt.sysarg[2],
            12 / 4 => return rt.sysarg[1],
            8 / 4 => return rt.sysarg[0],
            4 / 4 => return rt.sysres,
            else => {
                std.debug.print("Unimplemented read of I/O address {d}\n", .{adr});
                noreboExit(rt, 1);
            },
        }
    }

    inline fn writeWord(self: *Io, rt: *Runtime, adr: u32, val: u32) void {
        _ = self;
        const slot = @as(i32, @bitCast(adr));
        switch (@divTrunc(-slot, 4)) {
            60 / 4 => riscLeds(val),
            56 / 4 => {
                const out_byte: [1]u8 = .{@as(u8, @truncate(val))};
                _ = std.fs.File.stdout().writeAll(&out_byte) catch {};
            },
            16 / 4 => rt.sysarg[2] = val,
            12 / 4 => rt.sysarg[1] = val,
            8 / 4 => rt.sysarg[0] = val,
            4 / 4 => rt.sysres = sysreqExec(rt, val),
            else => {
                std.debug.print("Unimplemented write of I/O address {d}\n", .{adr});
                noreboExit(rt, 1);
            },
        }
    }
};

const Runtime = struct {
    mem: Memory = .{},
    sysarg: [3]u32 = .{ 0, 0, 0 },
    sysres: u32 = 0,
    nargc: u32 = 0,
    nargv: []const [:0]u8 = &.{},
    files: FileTable = .{},
    io: Io = .{},
    vm: Vm = .{},

    fn halt(self: *Runtime, ec: u32, _: u32, _: u32) u32 {
        noreboExit(self, @as(u8, @intCast(ec)));
    }

    fn argc(self: *Runtime, _: u32, _: u32, _: u32) u32 {
        return self.nargc;
    }

    fn argv(self: *Runtime, idx: u32, adr: u32, siz: u32) u32 {
        self.mem.checkRange(self, adr, siz, "Norebo.Argv");
        if (idx < self.nargc) {
            if (siz > 0) {
                const src = self.nargv[@as(usize, @intCast(idx))];
                const dst = self.mem.data[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + siz))];
                @memset(dst, 0);
                const len = @min(src.len, dst.len - 1);
                std.mem.copyForwards(u8, dst[0..len], src[0..len]);
            }
            return @as(u32, @intCast(self.nargv[@as(usize, @intCast(idx))].len));
        }
        return 0xFFFF_FFFF;
    }

    fn system(self: *Runtime, adr: u32, len: u32, _: u32) u32 {
        if (len == 0) return 0;
        self.mem.checkRange(self, adr, len, "Norebo.System");
        const allocator = std.heap.page_allocator;
        const slice = self.mem.data[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + len))];
        const buf = allocator.alloc(u8, slice.len + 1) catch {
            std.debug.print("Norebo.System: out of memory\n", .{});
            noreboExit(self, 1);
        };
        defer allocator.free(buf);
        std.mem.copyForwards(u8, buf[0..slice.len], slice);
        buf[slice.len] = 0;

        const sh_argv = [_][]const u8{ "/bin/sh", "-c", buf[0..slice.len] };
        var child = std.process.Child.init(&sh_argv, allocator);
        child.stdin_behavior = .Inherit;
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;
        const term = child.spawnAndWait() catch return 0xFFFF_FFFF;
        return switch (term) {
            .Exited => |code| @as(u32, @intCast(code)),
            else => 0xFFFF_FFFF,
        };
    }

    fn getenv(self: *Runtime, env_adr: u32, adr: u32, siz: u32) u32 {
        self.mem.checkRange(self, adr, siz, "OS.Getenv");
        var name_buf: [NameLength]u8 = undefined;
        self.mem.checkRange(self, env_adr, NameLength, "OS.Getenv");
        std.mem.copyForwards(u8, name_buf[0..], self.mem.data[@as(usize, @intCast(env_adr)) .. @as(usize, @intCast(env_adr + NameLength))]);
        const name = std.mem.sliceTo(&name_buf, 0);
        const value = std.process.getEnvVarOwned(std.heap.page_allocator, name) catch null;
        if (value) |val| {
            defer std.heap.page_allocator.free(val);
            if (siz > 0) {
                const dst = self.mem.data[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + siz))];
                @memset(dst, 0);
                const len = @min(val.len, dst.len - 1);
                std.mem.copyForwards(u8, dst[0..len], val[0..len]);
            }
            return 1;
        }
        return 0;
    }

    fn trap(self: *Runtime, trap_id: u32, name_adr: u32, pos: u32) u32 {
        var message: []const u8 = "unknown trap";
        switch (trap_id) {
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
        const name_ok = self.files.getName(self, &name_buf, name_adr);
        const name = if (name_ok) std.mem.sliceTo(&name_buf, 0) else "(unknown)";
        std.debug.print("{s} at {s} pos {d}\n", .{ message, name, pos });
        noreboExit(self, 100 + @as(u8, @intCast(trap_id)));
    }
};

fn tmpNameForHandle(hidx: usize, buf: *[64]u8) []const u8 {
    return std.fmt.bufPrint(buf, ".norebo_tmp_{d}", .{hidx}) catch ".norebo_tmp_invalid";
}

fn cleanupTempForHandle(hidx: usize) void {
    var tmp_name_buf: [64]u8 = undefined;
    const tmp_name = tmpNameForHandle(hidx, &tmp_name_buf);
    _ = std.fs.cwd().deleteFile(tmp_name) catch {};
}

fn cleanupTemps(rt: *Runtime) void {
    var hidx: usize = 0;
    while (hidx < MaxFiles) : (hidx += 1) {
        if (rt.files.entries[hidx].file != null and !rt.files.entries[hidx].registered) {
            rt.files.entries[hidx].file.?.close();
            rt.files.entries[hidx].file = null;
            cleanupTempForHandle(hidx);
            rt.files.entries[hidx] = .{};
        }
    }
}

fn cleanupTempFilesOnDisk() void {
    var it = std.fs.cwd().iterate();
    while (it.next() catch null) |ent| {
        if (std.mem.startsWith(u8, ent.name, TempPrefix)) {
            _ = std.fs.cwd().deleteFile(ent.name) catch {};
        }
    }
}

fn cleanupAllTemps(rt: *Runtime) void {
    cleanupTemps(rt);
    cleanupTempFilesOnDisk();
}

fn noreboExit(rt: *Runtime, code: u8) noreturn {
    cleanupAllTemps(rt);
    std.process.exit(code);
}

inline fn setRegister(rt: *Runtime, reg: u4, value: u32) void {
    rt.vm.r[reg] = value;
    rt.vm.flags.z = (value == 0);
    rt.vm.flags.n = (@as(i32, @bitCast(value)) < 0);
}

inline fn le32ToHost(ptr: []const u8) u32 {
    return @as(u32, ptr[0]) |
        (@as(u32, ptr[1]) << 8) |
        (@as(u32, ptr[2]) << 16) |
        (@as(u32, ptr[3]) << 24);
}

inline fn cpuReadProgram(rt: *Runtime, adr: u32) u32 {
    return rt.mem.readWord(rt, adr * 4);
}

inline fn cpuReadWord(rt: *Runtime, adr: u32) u32 {
    return if (@as(i32, @bitCast(adr)) >= 0) rt.mem.readWord(rt, adr) else rt.io.readWord(rt, adr);
}

inline fn cpuReadByte(rt: *Runtime, adr: u32) u32 {
    return if (@as(i32, @bitCast(adr)) >= 0) @as(u32, rt.mem.readByte(rt, adr)) else rt.io.readWord(rt, adr);
}

inline fn cpuWriteWord(rt: *Runtime, adr: u32, val: u32) void {
    if (@as(i32, @bitCast(adr)) >= 0) rt.mem.writeWord(rt, adr, val) else rt.io.writeWord(rt, adr, val);
}

inline fn cpuWriteByte(rt: *Runtime, adr: u32, val: u8) void {
    if (@as(i32, @bitCast(adr)) >= 0) rt.mem.writeByte(rt, adr, val) else rt.io.writeWord(rt, adr, val);
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

fn riscRun(rt: *Runtime) void {
    @setRuntimeSafety(false);
    while (true) {
        const ir = cpuReadProgram(rt, rt.vm.pc);
        rt.vm.pc +%= 1;

        if ((ir & Pbit) == 0) {
            const a: u4 = @intCast((ir >> 24) & 0xF);
            const b: u4 = @intCast((ir >> 20) & 0xF);
            const op: Op = @enumFromInt(@as(u4, @intCast((ir >> 16) & 0xF)));
            const im: u32 = ir & 0xFFFF;
            const c_reg: u4 = @intCast(ir & 0xF);

            const b_val = rt.vm.r[b];
            const c_val: u32 = if ((ir & Qbit) == 0)
                rt.vm.r[c_reg]
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
                            (@as(u32, @intFromBool(rt.vm.flags.n)) << 31) |
                            (@as(u32, @intFromBool(rt.vm.flags.z)) << 30) |
                            (@as(u32, @intFromBool(rt.vm.flags.c)) << 29) |
                            (@as(u32, @intFromBool(rt.vm.flags.v)) << 28);
                    } else {
                        a_val = rt.vm.h;
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
                    if ((ir & Ubit) != 0) tmp +%= @as(u32, @intFromBool(rt.vm.flags.c));
                    rt.vm.flags.c = tmp < b_val;
                    rt.vm.flags.v = (((tmp ^ c_val) & (tmp ^ b_val)) >> 31) != 0;
                    a_val = tmp;
                },
                .SUB => {
                    var tmp = b_val -% c_val;
                    if ((ir & Ubit) != 0) tmp -%= @as(u32, @intFromBool(rt.vm.flags.c));
                    rt.vm.flags.c = tmp > b_val;
                    rt.vm.flags.v = (((b_val ^ c_val) & (tmp ^ b_val)) >> 31) != 0;
                    a_val = tmp;
                },
                .MUL => {
                    if ((ir & Ubit) == 0) {
                        const tmp = @as(i64, @intCast(@as(i32, @bitCast(b_val)))) *
                            @as(i64, @intCast(@as(i32, @bitCast(c_val))));
                        a_val = @as(u32, @bitCast(@as(i32, @intCast(tmp))));
                        rt.vm.h = @as(u32, @bitCast(@as(i32, @intCast(tmp >> 32))));
                    } else {
                        const tmp: u64 = @as(u64, b_val) * @as(u64, c_val);
                        a_val = @as(u32, @intCast(tmp));
                        rt.vm.h = @as(u32, @intCast(tmp >> 32));
                    }
                },
                .DIV => {
                    if (@as(i32, @bitCast(c_val)) > 0) {
                        if ((ir & Ubit) == 0) {
                            a_val = @as(u32, @bitCast(@divTrunc(@as(i32, @bitCast(b_val)), @as(i32, @bitCast(c_val)))));
                            rt.vm.h = @as(u32, @bitCast(@rem(@as(i32, @bitCast(b_val)), @as(i32, @bitCast(c_val)))));
                            if (@as(i32, @bitCast(rt.vm.h)) < 0) {
                                a_val -%= 1;
                                rt.vm.h +%= c_val;
                            }
                        } else {
                            a_val = b_val / c_val;
                            rt.vm.h = b_val % c_val;
                        }
                    } else {
                        const q = idiv(b_val, c_val, (ir & Ubit) != 0);
                        a_val = q.quot;
                        rt.vm.h = q.rem;
                    }
                },
                .FAD => a_val = fpAdd(b_val, c_val, (ir & Ubit) != 0, (ir & Vbit) != 0),
                .FSB => a_val = fpAdd(b_val, c_val ^ 0x8000_0000, (ir & Ubit) != 0, (ir & Vbit) != 0),
                .FML => a_val = fpMul(b_val, c_val),
                .FDV => a_val = fpDiv(b_val, c_val),
            }
            setRegister(rt, a, a_val);
        } else if ((ir & Qbit) == 0) {
            const a: u4 = @intCast((ir >> 24) & 0xF);
            const b: u4 = @intCast((ir >> 20) & 0xF);
            var off: i32 = @bitCast(ir & 0x000F_FFFF);
            off = (off ^ 0x0008_0000) - 0x0008_0000;

            const address = rt.vm.r[b] +% @as(u32, @bitCast(off));
            if ((ir & Ubit) == 0) {
                const a_val = if ((ir & Vbit) == 0) cpuReadWord(rt, address) else cpuReadByte(rt, address);
                setRegister(rt, a, a_val);
            } else {
                if ((ir & Vbit) == 0) {
                    cpuWriteWord(rt, address, rt.vm.r[a]);
                } else {
                    cpuWriteByte(rt, address, @as(u8, @truncate(rt.vm.r[a])));
                }
            }
        } else {
            var t = ((ir >> 27) & 1) != 0;
            switch ((ir >> 24) & 7) {
                0 => t = t ^ rt.vm.flags.n,
                1 => t = t ^ rt.vm.flags.z,
                2 => t = t ^ rt.vm.flags.c,
                3 => t = t ^ rt.vm.flags.v,
                4 => t = t ^ (rt.vm.flags.c or rt.vm.flags.z),
                5 => t = t ^ (rt.vm.flags.n ^ rt.vm.flags.v),
                6 => t = t ^ ((rt.vm.flags.n ^ rt.vm.flags.v) or rt.vm.flags.z),
                7 => t = t ^ true,
                else => unreachable,
            }
            if (t) {
                if ((ir & Vbit) != 0) {
                    setRegister(rt, 15, rt.vm.pc * 4);
                }
                if ((ir & Ubit) == 0) {
                    const c_reg: u4 = @intCast(ir & 0xF);
                    rt.vm.pc = rt.vm.r[c_reg] / 4;
                } else {
                    var off: i32 = @bitCast(ir & 0x00FF_FFFF);
                    off = (off ^ 0x0080_0000) - 0x0080_0000;
                    rt.vm.pc = rt.vm.pc +% @as(u32, @bitCast(off));
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

const SysreqEntry = union(enum) {
    runtime: *const fn (*Runtime, u32, u32, u32) u32,
    files: *const fn (*FileTable, *Runtime, u32, u32, u32) u32,
};

const SysreqTableLen = 43;

fn initSysreqTable() [SysreqTableLen]?SysreqEntry {
    var table: [SysreqTableLen]?SysreqEntry = [_]?SysreqEntry{null} ** SysreqTableLen;
    table[1] = .{ .runtime = Runtime.halt };
    table[2] = .{ .runtime = Runtime.argc };
    table[3] = .{ .runtime = Runtime.argv };
    table[4] = .{ .runtime = Runtime.trap };
    table[11] = .{ .files = FileTable.new };
    table[12] = .{ .files = FileTable.old };
    table[13] = .{ .files = FileTable.register };
    table[14] = .{ .files = FileTable.close };
    table[15] = .{ .files = FileTable.seek };
    table[16] = .{ .files = FileTable.tell };
    table[17] = .{ .files = FileTable.read };
    table[18] = .{ .files = FileTable.write };
    table[19] = .{ .files = FileTable.length };
    table[20] = .{ .files = FileTable.date };
    table[21] = .{ .files = FileTable.delete };
    table[22] = .{ .files = FileTable.purge };
    table[23] = .{ .files = FileTable.rename };
    table[31] = .{ .files = FileTable.enumerateBegin };
    table[32] = .{ .files = FileTable.enumerateNext };
    table[33] = .{ .files = FileTable.enumerateEnd };
    table[41] = .{ .runtime = Runtime.getenv };
    table[42] = .{ .runtime = Runtime.system };
    return table;
}

const sysreq_table = initSysreqTable();

inline fn sysreqExec(rt: *Runtime, nval: u32) u32 {
    if (nval >= sysreq_table.len or sysreq_table[nval] == null) {
        std.debug.print("Unimplemented sysreq {d}\n", .{nval});
        noreboExit(rt, 1);
    }
    const entry = sysreq_table[nval].?;
    return switch (entry) {
        .runtime => |func| func(rt, rt.sysarg[0], rt.sysarg[1], rt.sysarg[2]),
        .files => |func| func(&rt.files, rt, rt.sysarg[0], rt.sysarg[1], rt.sysarg[2]),
    };
}

fn loadInnerCore(rt: *Runtime) void {
    var file: ?std.fs.File = std.fs.cwd().openFile(InnerCore, .{ .mode = .read_only }) catch null;
    if (file == null) {
        if (std.process.getEnvVarOwned(std.heap.page_allocator, PathEnv) catch null) |path_env| {
            defer std.heap.page_allocator.free(path_env);
            file = openFromPath(path_env, InnerCore, false);
        }
    }
    if (file == null) {
        std.debug.print("Can't load {s}\n", .{InnerCore});
        noreboExit(rt, 1);
    }
    var f = file.?;
    defer f.close();

    var siz = readUint32(&f) orelse unreachable;
    while (siz != 0) {
        const adr = readUint32(&f) orelse unreachable;
        rt.mem.checkRange(rt, adr, siz, InnerCore);
        const slice = rt.mem.data[@as(usize, @intCast(adr)) .. @as(usize, @intCast(adr + siz))];
        if (f.readAll(slice) catch 0 != slice.len) {
            std.debug.print("Error while reading {s}\n", .{InnerCore});
            noreboExit(rt, 1);
        }
        siz = readUint32(&f) orelse 0;
    }
}

pub fn main() !void {
    var rt: Runtime = .{};
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);
    defer cleanupAllTemps(&rt);
    if (args.len > 1) {
        rt.nargc = @as(u32, @intCast(args.len - 1));
        rt.nargv = args[1..];
    }

    loadInnerCore(&rt);
    rt.mem.writeWord(&rt, 12, MemBytes);
    rt.mem.writeWord(&rt, 24, StackOrg);
    rt.vm.pc = 0;
    rt.vm.r[12] = 0x20;
    rt.vm.r[14] = StackOrg;
    riscRun(&rt);
}
