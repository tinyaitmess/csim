.global _start
.global _exit

// Définition des adresses des périphériques et des constantes
.equ    LED,        0xA0000000  // Adresse de la LED
.equ    TC8BIT0,    0xFF202000  // Adresse de base du timer
.equ    TCNT0,      0x08        // Offset du registre TCNT0
.equ    TCCR0A,     0x44        // Offset du registre TCCR0A
.equ    TCCR0B,     0x45        // Offset du registre TCCR0B
.equ    OCR0A,      0x47        // Offset du registre OCR0A (pour mode CTC)
.equ    TIFR0,      0x35        // Offset du registre TIFR0
.equ    TIMSK0,     0x6E        // Offset du registre TIMSK0
.equ    DURATION,   0xFF        // Durée initiale du timer

_start:
    // Initialisation des registres
    ldr     R0, =LED            // Charger l'adresse de la LED dans R0
    ldr     R1, =TC8BIT0        // Charger l'adresse de base du timer dans R1

    add     R2, R1, #TCNT0      // R2 = adresse du registre TCNT0
    add     R3, R1, #TCCR0A     // R3 = adresse du registre TCCR0A
    add     R4, R1, #TCCR0B     // R4 = adresse du registre TCCR0B
    add     R5, R1, #OCR0A      // R5 = adresse du registre OCR0A
    add     R6, R1, #TIFR0      // R6 = adresse du registre TIFR0
    add     R7, R1, #TIMSK0     // R7 = adresse du registre TIMSK0

    mov     R11, #1             // R11 = état initial de la LED (allumée)
    mov     R10, #DURATION      // Charger la durée initiale dans R10
    strb    R10, [R2]           // Initialiser le compteur TCNT0 avec la durée (1 octet)

    // Configuration du registre TCCR0A (Mode Normal)
    mov     R10, #0x00          // TCCR0A = 0 (mode normal, aucun effet de compare)
    strb    R10, [R3]           // Écrire dans le registre TCCR0A

    // Configuration du registre TCCR0B (Mode CTC)
    mov     R10, #0b00001011    // Configurer le mode CTC (WGM02 = 1, CS02 = 1, CS01 = 0, CS00 = 1)
    strb    R10, [R4]           // Écrire dans le registre TCCR0B

    // Définir OCR0A pour définir la valeur de comparaison
    mov     R10, #0x50          // Fixer OCR0A à 0x50 pour la comparaison
    strb    R10, [R5]           // Écrire dans le registre OCR0A

    // Activation des interruptions par overflow (TOIE0)
    mov     R10, #0b00000001    // TOIE0 = 1 (autoriser les interruptions d'overflow)
    strb    R10, [R7]           // Écrire dans le registre TIMSK0

loop:
    // Lecture et gestion du timer
    ldrb    R10, [R6]           // Lire le registre TIFR0 (1 octet)
    tst     R10, #0b00000001    // Vérifier si le bit TOV0 (overflow) est activé
    beq     loop                // Si non, continuer à attendre

    mov     R10, #0b00000001    // Préparer la valeur pour réinitialiser TOV0
    strb    R10, [R6]           // Réinitialiser le flag TOV0 dans TIFR0 (1 octet)

    eor     R11, R11, #1        // Basculer l'état de la LED
    str     R11, [R0]           // Mettre à jour l'état de la LED (4 octets)

    // Vérification de l'overflow et réinitialisation du timer (si nécessaire)
    ldrb    R10, [R2]           // Lire la valeur de TCNT0
    cmp     R10, #DURATION      // Comparer TCNT0 avec la durée
    bge     reset_timer         // Si TCNT0 >= DURATION, réinitialiser le timer

    b       loop                // Retour à la boucle principale

reset_timer:
    mov     R10, #0x00          // Réinitialiser TCNT0 à 0
    strb    R10, [R2]           // Réécrire la valeur dans le registre TCNT0

    b       loop                // Revenir à la boucle principale

_exit:
    b       _exit               // Fin propre (bloque le coeur)
