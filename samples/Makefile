ARCH=arm-linux-gnueabihf-
CC=$(ARCH)gcc
CFLAGS=-g
LDFLAGS=-nostartfiles -static

all: sample1.elf sample2.elf

%.elf: %.s
	$(CC) $(CFLAGS) $< -o $@ $(LDFLAGS)
