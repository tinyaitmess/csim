-include config.mk

# Configuration
WITH_ARMV5T=1
YAML=$(PWD)/easy-yaml

HEADERS=csim.h
SOURCES=csim.c yaml.c led.c button.c csim-rt.o arm_core.c
#ARMV5T=armv5t

CFLAGS=-g3 -Wall -fPIC -I.
LDFLAGS=-L. -lcsim

# additional goals
ALL =
CLEAN =
DISTCLEAN =
ifdef WITH_STM32
ALL += stm32-all
CLEAN += stm32-clean
DISTCLEAN += stm32-distclean
endif

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
all: gliss-all libcsim.a test-csim test2 $(ALL)

clean: gliss-clean $(CLEAN)
	-rm -rf $(OBJECTS) test-csim.o test-csim

distclean: clean $(DISTCLEAN)
	-rm -rf test-csim test2 libcsim.a

test-csim: test-csim.o libcsim.a
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test2: test2.o libcsim.a
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

libcsim.a: $(OBJECTS)
	ar rcs $@ $(OBJECTS)

csim.o: csim.h mem.h
mem.o: mem.h
test-csim.o: csim.h
yaml.o: yaml.h
test2.o: csim.h mem.h yaml.h led.h button.h
led.o: led.h csim.h
button.o: button.h csim.h
csim-rt.o: csim-rt.h

FILES = \
	csim/README.md \
	csim/Makefile \
	csim/samples/Makefile \
	csim/samples/sample1.s \
	csim/samples/sample1.elf

dist: distclean
	cd ..; tar cvfz csim.tgz $(FILES) csim/*.c csim/*.h

gliss-all:
	cd gliss; make all

gliss-clean:
	cd gliss; make clean


# STM32 rules
stm32-all:
	cd stm32; make all

stm32-clean:
	cd stm32; make clean


# setup
GLISS_GIT = https://git.renater.fr/anonscm/git/gliss2/gliss2.git
ARMV5T_GIT = https://git.renater.fr/anonscm/git/gliss2/armv5t.git

setup: config.mk git-gliss git-armv5t

git-gliss:
	@if [ -e gliss2 ]; then \
		echo "Updating gliss"; \
		cd gliss2; git pull; \
	else \
		echo "Downloading gliss"; \
		git clone $(GLISS_GIT); \
	fi
	@cd gliss2; make

git-armv5t:
	@if [ -e armv5t ]; then \
		echo "Updating ArmV5T"; \
		cd armv5t; git pull; \
	else \
		echo "Downloading ArmV5T"; \
		git clone $(ARMV5T_GIT); \
		cd armv5t; \
		make config.mk; \
		echo "WITH_IO = 1" >> config.mk; \
	fi
	@cd armv5t; make

config.mk:
	cp config.in config.mk


