	.global _start
	.global _exit

_start:
loop:
	b	loop

_exit:
	b	_exit
