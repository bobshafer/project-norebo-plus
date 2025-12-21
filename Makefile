CFLAGS = -flto -Wall -Wextra -Wconversion -Wno-sign-conversion -Wno-unused-parameter -std=c99 -O3
ZFLAGS = -Doptimize=ReleaseFast -Dchecks=false -Dtarget=native

DEST=$(HOME)/bin

all: $(DEST)/nw zorebo

norebo.exe: Runtime/norebo.c Runtime/risc-cpu.c Runtime/risc-cpu.h
	$(CC) -o $@ Runtime/norebo.c Runtime/risc-cpu.c $(CFLAGS)

zorebo: Runtime/zorebo.zig Runtime/build.zig
	rm -f zorebo
	(cd Runtime && zig build $(ZFLAGS))
	ln Runtime/zig-out/bin/zorebo .

$(DEST)/nw: make-nw.sh
	sh make-nw.sh > $(DEST)/nw
	chmod a+x $(DEST)/nw

clean: clean-runtime clean-builds

clean-runtime:
	rm -f norebo.exe
	rm -f zorebo
	rm -rf Runtime/zig-out
	rm -rf Runtime/.zig-cache

clean-builds:
	rm -rf build1 build2 build3

