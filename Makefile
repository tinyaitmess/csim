
# Configuration
WITH_ARMV5T=1

HEADERS=csim.h
SOURCES=csim.c
ARMV5T=../armv5t

CFLAGS=-g3 -Wall -fPIC
LDFLAGS=-L. -lcsim

# ARMV5T option
ifdef WITH_ARMV5T
CFLAGS += -DNO_MEM -I$(ARMV5T)/include
LDFLAGS +=  -L$(ARMV5T)/src -larm
else
SOURCES += mem.c
endif


# useful definitions
OBJECTS=$(SOURCES:.c=.o)


# rules
all: libcsim.a test-csim test2

clean:
	-rm -rf $(OBJECTS) test-csim.o test-csim

distclean: clean
	-rm -rf test-csim libcsim.a

test-csim: test-csim.o libcsim.a
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test2: test2.o libcsim.a
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

libcsim.a: $(OBJECTS)
	ar rcs $@ $(OBJECTS)

csim.o: csim.h mem.h
mem.o: mem.h
test-csim.o: csim.h
