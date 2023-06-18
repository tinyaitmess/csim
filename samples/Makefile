ARCH=arm-linux-gnueabihf-
CC=$(ARCH)gcc
CFLAGS=
LDFLAGS=-nostartfiles -static

all: sample1.elf

sample1.elf: sample1.s
	$(CC) $(CFLAGS) $< -o $@ $(LDFLAGS)
