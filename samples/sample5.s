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
	mov R1, #0    // position

	cmp R1, #3
	addgt R10, R10, #0x10 //change the adress when writing to displays 4/5
loop:
	ldr	R2, [R11] // button value

	//set R0 to the value we want to display
	cmp R2, #0
	moveq R0, #0b00111111
	movne R0, #0b00000011


	cmp R1, #3	//check on which display we want to show
	lslle R0, R1 //if <4, we move the number by the display

	subgt R4, R0, #3 //else, we move by the number minus 3.
	lslgt R0, R4
	
	str R0, [R10]
	b	loop

_exit:
	b	_exit
