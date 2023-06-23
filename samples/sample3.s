	.global _start
	.global _exit

	.equ BUT, 0xB0000000

_start:
	ldr	r0, =BUT
	adr	r1, leds
	adr	r4, leds_end
	ldr	r2, [r1], #4
	mov	r3, #1
	str	r3, [r2]

loop:

wait_push:
	ldr	r3, [r0]
	cmp r3, #0
	bne	wait_push
wait_release:
	ldr	r3, [r0]
	cmp r3, #0
	beq	wait_release

	mov	r3, #0
	str	r3, [r2]
	cmp	r1, r4
	adreq r1, leds
	ldr	r2, [r1], #4
	mov	r3, #1
	str	r3, [r2]

	b	loop

_exit:
	b	_exit

leds:
	.word 0xA0000000, 0xA0010000, 0xA0020000, 0xA0030000
leds_end:

