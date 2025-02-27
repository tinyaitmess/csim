GLISS = ../gliss/gep/gliss-csim
CFLAGS = -I.. -g

NMPS = \
	GPIO.nmp

# -rm ${NMPS:.nmp=.c} ${NMPS:.nmp=.h} -- Si on ajoute les fichiers dans NPMS

all: libatmega328p.a

clean:
	-rm $(NMPS:.nmp=.o) libatmega328p.a
	-rm portd.h portd.c
	cd samples; make clean

distclean: clean
	-rm portd.h portd.c
	cd samples; make clean
	
libatmega328p.a: $(NMPS:.nmp=.o)
	ar rcs $@ $^

%.c: %.nmp
	$(GLISS) $<
	cd samples; make
