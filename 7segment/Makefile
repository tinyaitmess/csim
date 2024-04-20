GLISS = ../gliss/gep/gliss-csim
CFLAGS = -I.. -g

NMPS = seven_seg_controller.nmp seven_seg_display.nmp

all: $(NMPS:.nmp=.c)

clean:
	-rm -f $(NMPS:.nmp=.o) libstm32.a

distclean: clean
	-rm $(NMPS:.nmp=.h) $(NMPS:.nmp=.c)
	
libstm32.a: $(NMPS:.nmp=.o)
	ar rcs $@ $^

%.c: %.nmp
	$(GLISS) $<
