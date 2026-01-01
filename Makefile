CFLAGS = -flto -Wall -Wextra -Wconversion -Wno-sign-conversion -Wno-unused-parameter -std=c99 -O3
ZFLAGS = -Doptimize=ReleaseFast -Dchecks=false -Dtarget=native

DEST=$(HOME)/bin

all: $(DEST)/nw zorebo

norebo.exe: Runtime/norebo.c Runtime/risc-cpu.c Runtime/risc-cpu.h
	$(CC) -o $@ Runtime/norebo.c Runtime/risc-cpu.c $(CFLAGS)

zorebo: Runtime/zorebo.c
	$(CC) $(CFLAGS) -o $@ Runtime/zorebo.c

zoreboZ: RuntimeZig/zorebo.zig RuntimeZig/build.zig
	rm -f zoreboZ
	(cd RuntimeZig && zig build $(ZFLAGS))
	ln RuntimeZig/zig-out/bin/zorebo zoreboZ

zoreboA: RuntimeAda/alire.toml RuntimeAda/src/zorebo.adb
	rm -f zoreboA
	(cd RuntimeAda && alr build)
	ln RuntimeAda/obj/zorebo zoreboA

$(DEST)/nw: make-nw.sh
	sh make-nw.sh > $(DEST)/nw
	chmod a+x $(DEST)/nw

clean: clean-runtime clean-builds

clean-runtime:
	rm -f norebo.exe
	rm -f zorebo
	rm -rf RuntimeZig/zig-out
	rm -rf RuntimeZig/.zig-cache
	rm -rf RuntimeAda/obj

clean-builds:
	rm -rf build1 build2 build3

