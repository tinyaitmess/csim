GLISS = ../gliss/gep/gliss-csim
CFLAGS = -I.. -g

NMPS = $(wildcard */*.nmp)

all: $(NMPS:.nmp=.c)

clean:
	-rm -f $(wildcard *.c) $(wildcard *.h)
	
libstm32.a: $(NMPS:.nmp=.o)
	ar rcs $@ $^

%.c: %.nmp
	$(GLISS) $<
