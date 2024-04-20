	.global _start
	.global _exit

	.equ	LED,	0xFF200020
	.equ	BUT,	0xB0000000
	.equ 	ZERO,	0b00111111
	.equ 	ONE,	0b00000011

_start:
	ldr	R10, =LED // 7segment display
	ldr	R11, =BUT // button
	mov R0, #0b00111111 // displayed number
	mov R1, #3    // position at which it should be displayed

	cmp R1, #3
	addgt R10, R10, #0x10 //change the adress when writing to displays 4/5
loop:
	mov R6, R1
	ldr	R2, [R11] // button value

	//set R0 to the value we want to display
	cmp R2, #0
	moveq R0, #0b00111111
	movne R0, #0b00000011


	cmp R6, #3	//check on which display we want to show
	subgt R6, R6, #4 //if >4, we move by the number minus 3.
	mov R5, #8
	mul R4, R6, R5

	lsl R0, R4
	
	str R0, [R10]
	b	loop

_exit:
	b	_exit
