    .global _start
    .global _exit

    .equ LED,  0xA0000000 
    .equ BUT,  0xB0000000 
    .equ DDRD, 0x2A
    .equ PORTD, 0x2B 

_start:
    @ Configurer la LED (pin D0) en sortie
    ldr r0, =DDRD
    mov r1, #0b00000001       @ D0 en sortie
    strb r1, [r0]

loop:
    @ Lire l'état du bouton C
    ldr r0, =BUT
    ldr r1, [r0]       

    @ Tester si le bit 0 est actif (bouton pressé)
    tst r1, #0b00000001       
    beq eteindre_led          @ Si 0, éteindre LED

    @ Allumer LED
    ldr r0, =PORTD
    mov r1, #0b00000001
    strb r1, [r0]
    b loop

eteindre_led:
    ldr r0, =PORTD
    mov r1, #0b00000000
    strb r1, [r0]
    b loop

_exit:
    b _exit
