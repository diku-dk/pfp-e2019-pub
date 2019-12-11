# Set ISPC_TARGET to 'host' to use the best instructions for your
# machine.  Note that I have observed that results may differ from
# sequential execution for some targets.  I am not certain whether
# this is an ISPC bug.

ISPC_TARGET?=sse4
ISPC=ispc -O3 --target=$(ISPC_TARGET)
CC=gcc
CFLAGS=-O3 -std=c99 -Wall -Wextra -pedantic
PROGRAMS=mandelbrot sum relax filter scan pack rle

.PHONY: all run

all: $(PROGRAMS)

run: $(PROGRAMS) $(PROGRAMS:%=run_%)

run_%: % %.c
	./$*

$(PROGRAMS): %: %.c %.o %.h timing.h
	$(CC) -o $@ $(CFLAGS) $*.c $*.o

%.o %.h: %.ispc
	$(ISPC) $< -o $*.o -h $*.h

%.s: %.ispc
	$(ISPC) $< -o $*.s --emit-asm --x86-asm-syntax=intel

clean:
	rm -f $(PROGRAMS) $(PROGRAMS:=.o) $(PROGRAMS:=.h)
