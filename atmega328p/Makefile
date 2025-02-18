GLISS = ../gliss/gep/gliss-csim
CFLAGS = -I.. -g

NMPS = \
	GPIO.nmp

all: libatmega328p.a

clean:
	-rm $(NMPS:.nmp=.o) libatmega328p.a

distclean: clean
	-rm $(NMPS:.nmp=.h) $(NMPS:.nmp=.c)
	
libatmega328p.a: $(NMPS:.nmp=.o)
	ar rcs $@ $^

%.c: %.nmp
	$(GLISS) $<
