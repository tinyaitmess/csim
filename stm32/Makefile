GLISS = ../gliss/gep/gliss-csim
CFLAGS = -I.. -g

NMPS = \
	GPIO.nmp

all: libstm32.a

clean:
	-rm $(NMPS:.mp=.o) libstm32.a

distclean: clean
	-rm $(NMPS:.mp=.h) $(NMPS:.mp=.c)
	
libstm32.a: $(NMPS:.nmp=.o)
	ar rcs $@ $^

%.c: %.nmp
	$(GLISS) $<
