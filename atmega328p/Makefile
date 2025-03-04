GLISS = ../gliss/gep/gliss-csim
CFLAGS = -I.. -g

NMPS = \
	portd.nmp


all: libatmega328p.a

clean:
	-rm $(NMPS:.nmp=.o) libatmega328p.a
	cd samples; make clean
	
libatmega328p.a: $(NMPS:.nmp=.o)
	ar rcs $@ $^

%.c: %.nmp
	$(GLISS) $<
	mv $@ $(@:.c=.h) ../ 
	cd samples; make
