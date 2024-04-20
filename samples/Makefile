ARCH=arm-linux-gnueabihf-
CC=$(ARCH)gcc
CFLAGS=-g
LDFLAGS=-nostartfiles -static

all: sample1.elf sample2.elf sample3.elf sample4.elf sample5.elf

%.elf: %.s
	$(CC) $(CFLAGS) $< -o $@ $(LDFLAGS)
