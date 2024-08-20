GLISS = ../gliss/gep/gliss-csim
CFLAGS = -I.. -g

NMPS = \
	GPIO.nmp

all: libstm32.a

<<<<<<< HEAD
libstm32.a: $(NMPS:.nmp=.o)
	ar rcs $@ $^

*.c: *.nmp
	$(GLISS) $<

 
	
=======
clean:
	-rm $(NMPS:.nmp=.o) libstm32.a

distclean: clean
	-rm $(NMPS:.nmp=.h) $(NMPS:.nmp=.c)
	
libstm32.a: $(NMPS:.nmp=.o)
	ar rcs $@ $^

%.c: %.nmp
	$(GLISS) $<
>>>>>>> 265df0c48914834cd809f3b1fa4d510991f2033f
