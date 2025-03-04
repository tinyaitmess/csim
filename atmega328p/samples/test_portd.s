    .global _start
    .global _exit

    .equ LED,  0xA0000000 
    .equ BUT,  0xB0000000 
    .equ DDRD, 0x2A
    .equ PORTD, 0x2B 

_start:
    ldr r0, =DDRD
    mov r1, #0B001
    strb r1, [r0]
    ldr r0, =PORTD
    mov r1, #0B001      
    strb r1, [r0]

_exit:
    b _exit
    