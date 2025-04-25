	.global _start
	.global _exit

    .equ LED,  0xA0000000 
    .equ TIMER, 0xFF201000
    .equ DURATION, 0x01FF
    .equ TCCR1A, 0x80
    .equ TCCR1B, 0x81
    .equ TCCR1C, 0x82
    .equ TCNT1H, 0x85
    .equ TCNT1L, 0x84
    .equ OCR1BH, 0x8B
    .equ OCR1BL, 0x8A
    .equ ICR1H, 0x87
    .equ ICR1L, 0x86
    .equ TIMSK1, 0x6F
    .equ TIFR1, 0x36
    .equ OCR1AH, 0x89
    .equ OCR1AL, 0x88


_start :
    ldr	r0, =LED 
	ldr	r1, =TIMER
    add     R2, R1, #TCCR1A     // R2 = adresse de TCCR1A
    add     R3, R1, #TCCR1B     // R3 = adresse de TCCR1B
    add     R4, R1, #OCR1AH     // R4 = adresse de OCR1A High
    add     R5, R1, #OCR1AL     // R5 = adresse de OCR1A Low
    add     R6, R1, #TIFR1      // R6 = adresse de TIFR1
    add     R7, R1, #TIMSK1     // R7 = adresse de TIMSK1

    // Configuration du Timer1 en mode CTC
    mov     R10, #0x00          // TCCR1A = 0 (aucune action sur OC1A)
    strb    R10, [R2]           // Écrire dans TCCR1A

    mov     R10, #0b00001011    // TCCR1B = 0b00001011 : Mode CTC, prescaler 1024
    strb    R10, [R3]           // Écrire dans TCCR1B

    // Définir OCR1A pour définir la valeur de comparaison (ici OCR1A = 0x100)
    mov     R10, #0x01          // OCR1AH = 0x01 (High)
    strb    R10, [R4]           // Écrire dans OCR1AH
    mov     R10, #0x00          // OCR1AL = 0x00 (Low)
    strb    R10, [R5]           // Écrire dans OCR1AL

    // Configurer l'interruption pour le match de comparaison
    mov     R10, #0b00000010    // OCIE1A = 1 (interruption sur comparaison A)
    strb    R10, [R7]           // Écrire dans TIMSK1


_exit:
    b       _exit  
