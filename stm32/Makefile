GLISS = ../gliss/gep/gliss-csim
CFLAGS = -I.. -g

NMPS = \
	GPIO.nmp

all: libstm32.a

libstm32.a: $(NMPS:.nmp=.o)
	ar rcs $@ $^

*.c: *.nmp
	$(GLISS) $<

 
	
