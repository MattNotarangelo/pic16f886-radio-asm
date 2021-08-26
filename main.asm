;ELEC2117 Project by Matthew Notarangelo 17s1 z5116928

list    p=16f886	; list directive to define processor (PIC16F886)
#include	<p16f886.inc>	;processor specific variable definitions

;Define port names to make them easier to use and read from LED 8 segment display
#define	COL0	PORTA, 0
#define	COL1	PORTA, 1	
#define	COL2	PORTA, 2	
#define	COL3	PORTA, 3	
#define	ROW0	PORTA, 4	
#define	ROW1	PORTA, 5	
#define	ROW2	PORTA, 6	
#define	ROW3	PORTA, 7	

#define	LCD4 	PORTB, 0
#define	LCD5	PORTB, 1
#define	LCD6	PORTB, 2
#define	LCD7	PORTB, 3	
#define	LCD_RS	PORTC, 0	
#define	LCD_RW	PORTC, 1	
#define	LCD_EN	PORTC, 2
#define	SDIO	PORTC, 4
#define	SCLK	PORTC, 3
#define	RSTFM	PORTC, 7

#define	BAUD	D'100'      ; BAUD rate
#define	FOSC	D'4000'     ; Oscillation frequnecy

; '__CONFIG' directive is used to embed configuration data within .asm file.
; The labels following the directive are located in the respective .inc file.
; See respective data sheet for additional information on configuration word.

__CONFIG	_CONFIG1, _LVP_OFF & _FCMEN_ON & _IESO_OFF & _BOR_OFF & _CPD_OFF & _CP_OFF & _MCLRE_ON & _PWRTE_ON & _WDT_OFF & _INTRC_OSC_NOCLKOUT
__CONFIG	_CONFIG2, _WRT_OFF & _BOR21V

cblock	0x020	;Define variables
    COUNTERL
    COUNTERH
    INDEX
    d1
    d2
    d3
    d4
    d5
    d6
    counter
    chanH
    chanL
    dummy
    zerocol
    onecol
    twocol
    threecol
    threecolcheck
    zerocolcopy
    onecolcopy
    twocolcopy
    threecolcopy			
    SSPBUFbackup
    volumeReg
    muteReg
    screenReg
endc

;***** VARIABLE DEFINITIONS *****
w_temp	EQU	0x7D	; variable used for context saving
status_temp	EQU	0x7E	; variable used for context saving
pclath_temp	EQU	0x7F	; variable used for context saving
PC	EQU	0x02

;***** PROCESSOR INIT *****

ORG	0x000	; processor reset vector

nop
goto	main	; go to beginning of program

ORG	0x004	; interrupt vector location

movwf	w_temp	; save off current W register contents
movf	STATUS,w	; move status register into W register
movwf	status_temp	; save off contents of STATUS register
movf	PCLATH,w	; move pclath register into w register
movwf	pclath_temp	; save off contents of PCLATH register
movf	pclath_temp,w	; retrieve copy of PCLATH register
movwf	PCLATH	; restore pre-isr PCLATH register contents
movf	status_temp,w	; retrieve copy of STATUS register
movwf	STATUS	; restore pre-isr STATUS register contents
swapf	w_temp,f
swapf	w_temp,w	; restore pre-isr W register contents
retfie		; return from interrupt

main
    ; Setup memory addressing
	BANKSEL	ANSEL
	clrf	ANSEL

	BANKSEL	ANSELH
	clrf	ANSELH

    ; Setup clock frequency and port IO directions
	BANKSEL	TRISA
	movlw	B'11110000'
	movwf	TRISA

	BANKSEL	TRISB
	clrf	TRISB

	BANKSEL	TRISC
	clrf	TRISC

	BANKSEL	PORTA
	clrf	PORTA

	BANKSEL	PORTB
	clrf	PORTB

	BANKSEL	PORTC
	clrf	PORTC
	clrf	dummy
	movlw	B'00000011'   ;Preload volumeReg with default volume
	movwf	volumeReg
	movlw	B'11111111'   ;Preload muteReg with 1 (mute off)
	movwf	muteReg
	movwf	screenReg     ;Preload screenReg with 1 (screen on)
	clrw
	
initialisationLCD
	call	delay128ms
	bsf	LCD4
	bsf	LCD5
	bcf	LCD6   
	bcf	LCD7	
	
	call 	RunCommand	
	call	delay5ms ; Initalise 8 bits

	call 	RunCommand	
	call	delay5ms ; Initalise 8 bits
	
	call 	RunCommand	
	call	delay5ms ; Initalise 8 bits 
	
	bcf	LCD4
	bsf	LCD5
	bcf	LCD6
	bcf	LCD7 ; Initalise 4 bits

	call 	RunCommand	
	call	delay5ms

	bcf	LCD4
	bsf	LCD5
	bcf	LCD6   ;this part determines the Font, 1 or 2 lines, etc
	bcf	LCD7

	call 	RunCommand	
	call	CheckBF

	bcf	LCD4
	bcf	LCD5
	bcf	LCD6   ;;this part determines the Font, 1 or 2 lines, etc.
	bcf	LCD7;;;;

	call 	RunCommand	
	call	CheckBF

	call 	RunCommand	
	call	CheckBF
	
	bcf	LCD4
	bcf	LCD5
	bsf	LCD6
	bsf	LCD7 ; display, cursor, blink

	call 	RunCommand	
	call	CheckBF

	bcf	LCD4
	bcf	LCD5	; set shift mode
	bcf	LCD6
	bcf	LCD7

	call 	RunCommand	
	call	CheckBF

	bcf	LCD7
	bsf	LCD6 ; set shift mode
	bsf	LCD5 
	bcf	LCD4
	
	call 	RunCommand	
	call	CheckBF


	bcf	LCD7
	bcf	LCD6
	bcf	LCD5
	bcf	LCD4
	
	call 	RunCommand	
	call	CheckBF

	bcf	LCD7
	bcf	LCD6
	bcf	LCD5
	bsf	LCD4 ; clear display and return to home

	call 	RunCommand	
	call	CheckBF


I2CInitialisation
	BANKSEL     PORTC
	bsf	RSTFM

	BANKSEL	TRISC
	bsf	TRISC, 3
	bsf	TRISC, 4	

	call	delay512ms

	BANKSEL	SSPCON
	movlw	B'00101000'	;set to i2c master mode
	movwf	SSPCON	

	BANKSEL	SSPSTAT
	movlw	B'10000000'	;turn on i2c
	movwf	SSPSTAT
	
	BANKSEL SSPADD
	movlw (FOSC / (4 * BAUD)) - 1
	movwf	SSPADD
	
	call	delay512ms	;delay to let it initialise

I2CWrite
	call	SendControlStart  ;start

	movlw	B'00000000'	;02H
	call	Transmit
	
	movlw	B'00000000'	;02L
	call	Transmit
	
	movlw	B'00000000'	;03H
	call	Transmit
	
	movlw	B'00000000'	;03L
	call	Transmit

	movlw	B'00000000'	;04H
	call	Transmit

	movlw	B'00000000'	;04L
	call	Transmit
	
	movlw	B'00000000' 	;05H
	call	Transmit

	movlw	B'00000000' 	;05L
	call	Transmit

	movlw	B'00000000' 	;06H
	call	Transmit

	movlw	B'00000000' 	;06L
	call	Transmit

	movlw	B'10000001'	;07H	;turn on crystal
	call	Transmit

	movlw	B'00000000'	;07L	;turn on crystal
	call	Transmit

	call	SendStop
	call	delay512ms	;let crystal stabilise

;start again
	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L	;turn on the chip
	call	Transmit
	
;stop
	call	SendStop
	call	delay128ms	;let it stabilise

;start again 2
	call	SendControlStart

	movlw	B'11000000'	;02H	;initialise registers with correct band/spacing
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'00000000'	;03H
	call	Transmit
	
	movlw	B'00000000'	;03L
	call	Transmit
	
	movlw	B'00011000'	;04H
	call	Transmit
	
	movlw	B'00000000'	;04L
	call	Transmit

	movlw	B'00000000'	;05H
	call	Transmit
	
	movlw	B'00000011'	;05L
	call	Transmit

	call	SendStop

	call	delay512ms

	call	OnePress	;goes to the first preset
	goto	check_keypad	;goes to check keypad loop
					
delay128ms
	BANKSEL	PORTB
	movlw	0xFE
	movwf	d1
	movlw	0x64	
	movwf	d2

Delay_0
	decfsz	d1, f	
	goto	$+2	
	decfsz	d2, f	
	goto	Delay_0			
	goto	$+1
	nop	
	return

delay5ms
	BANKSEL	PORTB
	movlw	0xE6	
	movwf	d1	
	movlw	0x04	
	movwf	d2

Delay_1
	decfsz	d1, f	
	goto	$+2	
	decfsz	d2, f	
	goto	Delay_1			;3 cycles
	
	goto	$+1
	nop                     ;4 cycles (including call)
	
	return

delay512ms
	call	delay128ms
	call	delay128ms
	call	delay128ms
	call	delay128ms
	
	return

CheckBF 
	call	delay5ms
	return

RunCommand
	BANKSEL	PORTB
	bsf	LCD_EN
	bcf	LCD_EN
	return

check_keypad				
	BANKSEL	PORTB

	bsf COL0					
	btfsc ROW0			;	has the 1 key been pressed?
	call	OnePress				
	btfsc ROW1			;	has the 4 key been pressed? 
	call	FourPress				
	btfsc ROW2			;	has the 7 key been pressed? 
	call	SevenPress			
	btfsc ROW3			;	has the * key been pressed?
	call	AsteriskPress			
	bcf COL0			;	

	bsf COL1			
	btfsc ROW0			;	has the 2 key been pressed?
	call	TwoPress			
	btfsc ROW1			;	has the 5 key been pressed? 
	call	FivePress				
	btfsc ROW2			;	has the 8 key been pressed? 
	call	EightPress			
	btfsc ROW3			;	has the 0 key been pressed? 
	Call	ZeroPress				
	bcf COL1			;	

	bsf COL2			;	
	btfsc ROW0			;	has the 3 key been pressed?
	call	ThreePress			;
	btfsc ROW1			;	has the 6 key been pressed? 
	call	SixPress			;.
	btfsc ROW2			;	has the 9 key been pressed? 
	call	NinePress				;
	btfsc ROW3			;	has the # key been pressed? 
	call	HashPress				;
	bcf COL2
			;	
	bsf COL3			;			
	btfsc ROW0			;	has the A key been pressed? 
	call	APressThatIsntReset				
	btfsc ROW1			;	has the B key been pressed? 
	call	BPress				
	btfsc ROW2			;	has the C key been pressed? 
	call	CPress			;	
	btfsc ROW3			;	has the D key been pressed? 
	call	DPress				
	bcf COL3			;	
	
	call	delay128ms	;this delay acts as a debounce		
	goto	check_keypad					

APressThatIsntReset  ;turn screen on/off - toggles. Uses screenReg, 0 for off, 1 if it's on
	btfss	ROW0 ;makes sure that A was actually pressed
	return
	bcf	LCD_RS

	bcf	LCD4   
	bcf	LCD5
	bcf	LCD6
	bcf	LCD7
	call 	RunCommand 
	call	CheckBF

	bcf	LCD4
	bcf	LCD5
	btfsc	screenReg, 0   ;turns off if it is on
	bcf	LCD6
	btfss	screenReg, 0	;turns on if it is off
	bsf LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	comf	screenReg	;complement screenReg to signal on/off
	call	delay128ms
	return

ZeroPress   ;no operation function; 
	return

OnePress            ;station 1   96.9
	BANKSEL	PORTB
	btfss	muteReg, 0
	return

	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'10000000'	;03H
	call	Transmit
	
	movlw	B'00101111'	;03L
	call	Transmit

	call	SendStop
	call	waitFunction
	call	I2CRead 
	return

TwoPress	        ;station 2   smoooooooooooth 95.3
	BANKSEL	PORTB
	btfss	muteReg, 0
	return

	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'10000000'	;03H
	call	Transmit
	
	movlw	B'00100111'	;03L
	call	Transmit

	call	SendStop
	call	waitFunction
	call	I2CRead
	return

ThreePress	        ;station 3   104.1
	BANKSEL	PORTB
	btfss	muteReg, 0
	return

	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'10000000'	;03H
	call	Transmit
	
	movlw	B'01010011'	;03L
	call	Transmit

	call	SendStop
	call	waitFunction
	call	I2CRead
	return

FourPress	        ;station 4  101.7
	BANKSEL	PORTB
	btfss	muteReg, 0
	return

	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'10000000'	;03H
	call	Transmit
	
	movlw	B'01000111'	;03L
	call	Transmit

	call	SendStop
	call	waitFunction
	call	I2CRead
	return
	
FivePress	        ;station 5   106.5
	BANKSEL	PORTB
	btfss	muteReg, 0
	return

	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'10000000'	;03H
	call	Transmit
	
	movlw	B'01011111'	;03L
	call	Transmit
	
	call	SendStop
	call	waitFunction
	call 	I2CRead	
	return

SixPress            ;station 6  104.9
	BANKSEL	PORTB
	btfss	muteReg, 0
	return

	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'10000000'	;03H
	call	Transmit
	
	movlw	B'01010111'	;03L
	call	Transmit
	
	call	SendStop
	call	waitFunction
	call 	I2CRead	
	return
	
SevenPress	        ;station 7  96.1 
	BANKSEL	PORTB
	btfss	muteReg, 0
	return

	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'10000000'	;03H
	call	Transmit
	
	movlw	B'00101011'	;03L
	call	Transmit
	
	call	SendStop
	call	waitFunction
	call 	I2CRead	
	return

EightPress	            ;FBi	94.5
	BANKSEL	PORTB
	btfss	muteReg, 0
	return
	
	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'10000000'	;03H
	call	Transmit
	
	movlw	B'00100011'	;03L
	call	Transmit
	
	call	SendStop
	call	waitFunction
	call 	I2CRead	
	return

NinePress               ; ABC Classic FM 92.9
	BANKSEL	PORTB
	btfss	muteReg, 0
	return
	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'10000000'	;03H
	call	Transmit
	
	movlw	B'00011011'	;03L
	call	Transmit
	
	call	SendStop
	call	waitFunction
	call 	I2CRead	
	return

AsteriskPress           ;Seek Down
	BANKSEL	PORTB
	btfss	muteReg, 0
	return

	call	SendControlStart

	movlw	B'11000001'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit
	
	call	SendStop
	call	waitFunction
	call 	I2CRead
	return
	
HashPress	            ;Seek Up
	BANKSEL	PORTB
	btfss	muteReg, 0
	return

	call	SendControlStart

	movlw	B'11000011'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit
	
	call	SendStop
	call	waitFunction
	call 	I2CRead
	return
	
DPress	
;mute/unmute
;0 in muteReg means mute is on (no sound is coming out), 1 mute is off (so sound is being outputted)
	BANKSEL	PORTB
	btfss	muteReg, 0
	goto	turnMuteOff
	goto	turnMuteOn
	
turnMuteOff
	call	SendControlStart
	movlw	B'11000000'	;02H
	goto	EndMuteFunction

turnMuteOn
	call	SendControlStart
	movlw	B'10000000'	;02H

EndMuteFunction
	call	Transmit
	call	SendStop
	
	BANKSEL	PORTB
	comf	muteReg, 1
	call 	APress
	btfsc	muteReg, 0
	call 	I2CRead
	btfss 	muteReg, 0
	call 	displayMute

	return

CPress	            ;decrement the volume
	BANKSEL	PORTB
	btfss	muteReg, 0
	return
	decf	volumeReg, 1
	btfsc	volumeReg, 4   ;checks for overflow
	goto 	VolumeAlreadyAtMin

	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'00000000'	;03H
	call	Transmit
	
	movlw	B'00000000'	;03L
	call	Transmit
	
	movlw	B'00011000'	;04H
	call	Transmit
	
	movlw	B'00000000'	;04L
	call	Transmit

	movlw	B'00000000'	;05H
	call	Transmit
	
	BANKSEL	PORTB
	movf	volumeReg, 0	;05L
	call	Transmit

	call	SendStop
	call 	displayVolumeDown

endOfDownVolFunction    ; no operation
	return

VolumeAlreadyAtMin
	incf 	volumeReg, 1
	call 	displayVolumeAtMin
	goto	endOfDownVolFunction
	return
	
BPress	            ;increment the volume
	BANKSEL	PORTB
	btfss	muteReg, 0
	return
	incf	volumeReg, 1
	btfsc	volumeReg, 4	;checks for overflow
	goto 	VolumeAlreadyAtMax

	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'00000000'	;03H
	call	Transmit
	
	movlw	B'00000000'	;03L
	call	Transmit
	
	movlw	B'00011000'	;04H
	call	Transmit
	
	movlw	B'00000000'	;04L
	call	Transmit

	movlw	B'00000000'	;05H
	call	Transmit
	
	BANKSEL	PORTB
	movf	volumeReg, 0	;05L
	call	Transmit

	call	SendStop
	call 	displayVolumeUp

endOfUpVolFunction  ; no operation
	return

VolumeAlreadyAtMax
	decf 	volumeReg, 1
	call 	displayVolumeAtMax
	goto	endOfUpVolFunction
	return

APress				;;;RESET;;;CLEAR AND MOVE TO HOME;;;;
;This button resets the display
	bcf	LCD_RS

	bcf	LCD4
	bcf	LCD5
	bcf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bcf	LCD6
	bcf	LCD7
	
	call 	RunCommand
	call	CheckBF
	return	

I2CFail ;error handling
	BANKSEL	SSPCON2
	bsf	SSPCON2, PEN
	call	WaitMSSP

	goto $
	return

Send_I2C_Byte
	BANKSEL	SSPBUF
	movwf	SSPBUF
	retlw	0

WaitMSSP
	BANKSEL	PIR1
	btfss	PIR1, SSPIF    ;SSPIF checks whether an I2c operation is in progress
	goto	$-1
	bcf	PIR1, SSPIF
	BANKSEL	PORTB
	clrw
	return
	
waitFunction
	call	delay128ms
	call	SendControlStart

	movlw	B'11000000'	;02H
	call	Transmit
	
	movlw	B'00000001'	;02L
	call	Transmit

	movlw	B'00000000'	;03H     ;clears seek/tune bit by overwriting with 0
	call	Transmit
	
	movlw	B'00000000'	;03L
	call	Transmit

	call	SendStop
	call	delay128ms
	return

SendControlStart
	BANKSEL	SSPCON2
	bsf	SSPCON2, SEN
	
	call	WaitMSSP

	movlw	B'00100000'	;start  ;address of si4703
	call	Transmit

	return

SendStop
	BANKSEL	SSPCON2
	bsf	SSPCON2,PEN 
	call 	WaitMSSP 
	return

Transmit    ; Send via I2C
	BANKSEL	PORTB
	movwf	SSPBUFbackup

retryBF1
	BANKSEL	PORTB
	movf	SSPBUFbackup ,0	
	BANKSEL	SSPCON
	bcf	SSPCON, 7
	BANKSEL	SSPSTAT
	btfsc	SSPSTAT, 0     ;SSPSTAT, 0 is the busy flag
	goto	retryBF1
	call	Send_I2C_Byte
	call	WaitMSSP
	BANKSEL	SSPCON
	btfsc	SSPCON, 7      ;SSPCON, 7 is the write collision bit
	goto	retryBF1
	
	BANKSEL	SSPCON2
	btfsc	SSPCON2,ACKSTAT
	goto	I2CFail
	
	return

I2CRead
	BANKSEL	SSPBUF
	clrf	SSPBUF
	BANKSEL	PORTB
	clrw

	clrf	chanH	;clear all variables
	clrf	chanL
	clrf	dummy
	clrf	counter
	clrf	onecol
	clrf	twocol
	clrf	threecol
	clrf	zerocol
	clrf	threecolcheck
	clrf	zerocolcopy
	clrf	onecolcopy
	clrf	twocolcopy
	clrf	threecolcopy

	BANKSEL 	SSPCON2
	bsf     	SSPCON2,SEN    ; Generate start Condition
	call    	WaitMSSP        ; Wait for I2C operation to start

	movlw   	B'00100001'     ; Load CONTROL BYTE (output) The 0010000 part tells to write to the fm chip, while the 0/1 on the end says read/write
	call    	Transmit	;send it

	BANKSEL	SSPBUF
	movf	SSPBUF, 0
	movwf	dummy     ;You have to read from SSPBUF after each read otherwise it can't write anymore
	call	CheckProg
	BANKSEL 	SSPCON2
	
	bsf 	SSPCON2,RCEN ; Enable Receive Mode (I2C)     ;Enabling Recieve mode tells the fm chip that the pic is ready to receive info
	
	call 	WaitMSSP     ;wait for it to complete
	call	CheckProg

	BANKSEL 	SSPCON2
	bcf     	SSPCON2,ACKDT   ; ACK DATA to send is 0	;0Ah       ;this line loads an acknowledge to send to the fm chip
	bsf     	SSPCON2,ACKEN   ; Send ACK DATA now.		;this line sends the ack

	BANKSEL	SSPBUF
	movf	SSPBUF, 0
	movwf	dummy			;again read from the buffer
	call	CheckProg
	
	BANKSEL 	SSPCON2
	
	bsf 	SSPCON2,RCEN ; Enable Receive Mode (I2C)    ;The receive mode turns off after each cycle
	
	call 	WaitMSSP
	call	CheckProg

	BANKSEL 	SSPCON2
	bcf     	SSPCON2,ACKDT   ; ACK DATA to send is 0	;0Al
	bsf     	SSPCON2,ACKEN   ; Send ACK DATA now.

	BANKSEL	SSPBUF
	movf	SSPBUF, 0
	movwf	dummy
	call	CheckProg
	BANKSEL 	SSPCON2
	
	bsf 	SSPCON2,RCEN ; Enable Receive Mode (I2C
	
	call 	WaitMSSP
	call	CheckProg

	BANKSEL 	SSPCON2
	bcf     	SSPCON2,ACKDT   ; ACK DATA to send is 0	;0Bh
	bsf     	SSPCON2,ACKEN   ; Send ACK DATA now.

	BANKSEL	SSPBUF
	movf	SSPBUF, 0
	movwf	chanH
	call	CheckProg

	BANKSEL 	SSPCON2
	
	bsf 	SSPCON2,RCEN ; Enable Receive Mode (I2C
	
	call 	WaitMSSP
	call	CheckProg

	BANKSEL	SSPBUF
	movf	SSPBUF, 0
	movwf	chanL
	call	CheckProg

	BANKSEL 	SSPCON2
	bsf     	SSPCON2,ACKDT   ; NACK DATA to send is 1	;0Bl
	bsf     	SSPCON2,ACKEN   ; Send ACK DATA now.

	call	delay128ms

retry
	call	SendStop
	BANKSEL	SSPSTAT
	btfss	SSPSTAT, 4   
	goto	retry
	
	call	delay5ms
	
	BANKSEL	PORTB
	movlw	D'05'
	movwf	zerocol
	movlw	D'07'
	movwf	onecol
	movlw	D'08'
	movwf	twocol
	movf	chanL, 0
	movwf	counter
	
freqCalc        ;freq = (2*readchan + 875) /10
	clrw
	decf	counter, 1
	incf	zerocol, 1
	call	checkzerocol
	call	checkonecol
	call	checktwocol
	
	incf	zerocol, 1
	call	checkzerocol
	call	checkonecol
	call	checktwocol
	
	clrw
	movlw	B'00000000'  ;check if the counter is at 0
	addwf	counter, 1
	btfss	STATUS, Z
	goto	freqCalc
	
	call	APress
	bsf	LCD_RS
	
	btfsc	threecolcheck, 0   ;third col can only be 0 or 1 since fm radio frequencies don’t go high enough
	call	PrintThreeCol
	
	clrw
	movlw	D'48'
	addwf	twocol, 1
	movf	twocol, 0
	movwf	twocolcopy
	rrf	twocol, 1
	rrf	twocol, 1
	rrf	twocol, 1
	rrf	twocol, 1
	movf	twocol, 0
	movwf	PORTB

	call 	RunCommand
	call	CheckBF
	movf	twocolcopy, 0 
	movwf	PORTB

	call 	RunCommand
	call	CheckBF
	
	clrw
	movlw	D'48'
	addwf	onecol, 1
	movf	onecol, 0
	movwf	onecolcopy
	rrf	onecol, 1
	rrf	onecol, 1
	rrf	onecol, 1
	rrf	onecol, 1
	movf	onecol, 0
	movwf	PORTB

	call 	RunCommand
	call	CheckBF
	movf	onecolcopy, 0
	movwf	PORTB

	call 	RunCommand
	call	CheckBF
	
	bcf	LCD4
	bsf	LCD5
	bcf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bsf	LCD5
	bsf	LCD6
	bsf	LCD7

	call 	RunCommand
	call	CheckBF
	
	clrw
	movlw	D'48'
	addwf	zerocol, 1
	movf	zerocol, 0
	movwf	zerocolcopy
	rrf	zerocol, 1
	rrf	zerocol, 1
	rrf	zerocol, 1
	rrf	zerocol, 1
	movf	zerocol, 0
	movwf	PORTB

	call 	RunCommand
	call	CheckBF
	movf	zerocolcopy, 0
	movwf	PORTB

	call 	RunCommand
	call	CheckBF
	
	bcf	LCD4
	bcf	LCD5      ;print M
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bsf	LCD6
	bsf	LCD7

	call 	RunCommand
	call	CheckBF

	bcf	LCD4
	bcf	LCD5  ;printH
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bcf	LCD5
	bcf	LCD6
	bsf	LCD7

	call 	RunCommand
	call	CheckBF	

	bsf	LCD4
	bsf	LCD5 ;;print z
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bsf	LCD5
	bcf	LCD6
	bsf	LCD7

	call 	RunCommand
	call	CheckBF
	bcf	LCD_RS

	BANKSEL	PORTB
	clrf	zerocol
	clrf	onecol
	clrf	twocol
	clrf	threecol
		
	return

checkzerocol
	movf	zerocol, 0
	sublw	D'10'
	btfsc	STATUS, Z
	goto	SetOneColHigh
	return

SetOneColHigh	
	movlw	D'01'
	addwf	onecol, 1
	movlw	D'10'
	subwf	zerocol, 1
	return
	
checkonecol
	movf	onecol, 0
	sublw	D'10'
	btfsc	STATUS, Z
	goto	SetTwoColHigh
	return

SetTwoColHigh	
	movlw	D'01'
	addwf	twocol, 1
	movlw	D'10'
	subwf	onecol, 1
	return
	
checktwocol
	movf	twocol, 0
	sublw	D'10'
	btfsc	STATUS, Z
	goto	SetThreeColHigh
	return

SetThreeColHigh	
	movlw	D'01'
	addwf	threecol, 1
	movlw	D'10'
	subwf	twocol, 1
	bsf	threecolcheck, 0
	return

PrintThreeCol	
	clrw
	movlw	D'48'
	addwf	threecol, 1
	movf	threecol, 0
	movwf	threecolcopy
	rrf	threecol, 1
	rrf	threecol, 1
	rrf	threecol, 1
	rrf	threecol, 1
	movf	threecol, 0
	movwf	PORTB

	call 	RunCommand
	call	CheckBF
	movf	threecolcopy, 0
	movwf	PORTB

	call 	RunCommand
	call	CheckBF
	
	return

CheckProg
	BANKSEL	SSPSTAT
	btfsc	SSPSTAT, 2  ;check whether the transmission is in progress
	goto	$-1
	BANKSEL SSPCON2
	btfsc SSPCON2,RCEN	;check whether RCEN has gone back to low since it resets after each transmission
	goto $-1
	return

displayVolumeDown  ;prints vol-
	call 	APress ; reset screen

	bsf	LCD_RS

	bsf	LCD4   ;v
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;o
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bsf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;l
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bcf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;-
	bsf	LCD5
	bcf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD_RS

	call 	delay512ms
	call 	I2CRead
	return

displayVolumeUp   ;prints vol+
	call 	APress ; reset screen

	bsf	LCD_RS
	bsf	LCD4   ;v
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;o
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bsf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;l
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bcf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;+
	bsf	LCD5
	bcf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bsf	LCD5
	bcf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD_RS

	call 	delay512ms
	call 	I2CRead
	return

displayVolumeAtMax   ;prints VolAtMax
	call 	APress ; reset screen

	bsf	LCD_RS
	bsf	LCD4   ;V
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;o
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bsf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;l
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bcf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;A
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bcf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bsf	LCD4  ;t
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;M
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;a
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bcf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bsf	LCD4  ;x
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bcf	LCD5
	bcf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD_RS

	call 	delay512ms
	call 	I2CRead
	return

displayVolumeAtMin   ;prints VolAtMin
	call 	APress ; reset screen

	bsf	LCD_RS

	bsf	LCD4   ;V
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;o
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bsf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;l
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bcf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;A
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bcf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bsf	LCD4  ;t
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;M
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;i
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bcf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;n
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bsf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD_RS

	call 	delay512ms
	call 	I2CRead
	return

displayMute   ;prints VolAtMin
	bsf	LCD_RS

	bcf	LCD4  ;M
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bsf	LCD6
	bsf	LCD7
	call 	RunCommand
	call	CheckBF

	bsf	LCD4  ;u
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bsf	LCD4  ;t
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bcf	LCD4
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD4  ;e
	bsf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF
	bsf	LCD4
	bcf	LCD5
	bsf	LCD6
	bcf	LCD7
	call 	RunCommand
	call	CheckBF

	bcf	LCD_RS

	return

	END	; directive 'end of program'