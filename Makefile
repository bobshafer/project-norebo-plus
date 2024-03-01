CFLAGS = -flto -Wall -Wextra -Wconversion -Wno-sign-conversion -Wno-unused-parameter -std=c99 -O3

DEST=$(HOME)/bin

all: $(DEST)/nw zorebo

norebo.exe: Runtime/norebo.c Runtime/risc-cpu.c Runtime/risc-cpu.h
	$(CC) -o $@ Runtime/norebo.c Runtime/risc-cpu.c $(CFLAGS)

zorebo: Runtime/zorebo.c
	$(CC) -o $@ Runtime/zorebo.c $(CFLAGS)

$(DEST)/nw: make-nw.sh
	sh make-nw.sh > $(DEST)/nw
	chmod a+x $(DEST)/nw

clean: clean-runtime clean-builds

clean-runtime:
	rm -f norebo.exe
	rm -f zorebo

clean-builds:
	rm -rf build1 build2 build3
