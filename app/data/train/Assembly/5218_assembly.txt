;*******************************************************************************
;                                                                              *
;    Filename: main.asm                                                        *
;    Date: 8th December 2015                                                   *
;    File Version: 1                                                           *
;    Author: Paul Winkler                                                      *
;    Company: -                                                                *
;    Description: Safe cracking servo control                                  *
;                                                                              *
;*******************************************************************************

#include "p10F206.inc"

; CONFIG
; __config 0xFFEB
 __CONFIG _WDTE_OFF & _CP_OFF & _MCLRE_OFF
 

	cblock 0x08
servo_period
delay_counter
delay_counter_inner
dwell_period

	endc
 DWELL_TIME EQU D'12'
	
 SERVO_TOTAL EQU D'200'
 SERVO_POS1 EQU	D'18'
 SERVO_POS2 EQU	D'12'
 KILL_PIN EQU 0
 MOTOR_PIN EQU 1
 SERVO_PIN EQU 2
 TRIGGER_PIN EQU 3
 TRIS_PINS EQU b'11111001'
	
;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector
	GOTO    START                  ; go to beginning of program


;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************

MAIN_PROG CODE                      ; let linker place main program

START

	CLRF	CMPOUT
	MOVLW	b'11110011'
	MOVWF	CMCON0
	MOVLW	b'00000000'
	MOVWF	OSCCAL
	MOVLW	b'11000000'
	OPTION
	MOVLW	TRIS_PINS
	TRIS	GPIO
	CLRF	GPIO
RESET
	MOVLW	SERVO_POS1
	MOVWF	servo_period
	CLRF	dwell_period
	
	BSF	GPIO, MOTOR_PIN
MAIN_LOOP

	; check kill switch
	BTFSS	GPIO, KILL_PIN
	GOTO	MAIN_END_KILL
	BCF	GPIO, MOTOR_PIN
MAIN_KILL
	GOTO MAIN_KILL ; loop forever
MAIN_END_KILL
	
	; check switch and set servo to position 2
	BTFSC	GPIO, TRIGGER_PIN
	GOTO	SET_POS_2
	
	; if no dwel time then loop
	MOVF	dwell_period, f
	BTFSC	STATUS, Z
	GOTO	MAIN_LOOP

	; decrement dwell time and output to servo if not zero
	DECFSZ	dwell_period, f
	GOTO	SERVO_OUTPUT

	; end of dwell, if it is at pos 1 then go back to loop
	MOVLW	SERVO_POS1
	SUBWF	servo_period, w
	BTFSC	STATUS, Z
	GOTO	MAIN_LOOP
	; else return to pos 1

SET_POS_1
	MOVLW	SERVO_POS1
	GOTO	START_SERVO_OUTPUT

SET_POS_2
	MOVLW	SERVO_POS2

START_SERVO_OUTPUT
	MOVWF	servo_period
	MOVLW	DWELL_TIME
	MOVWF	dwell_period
	
SERVO_OUTPUT
	; set high of pwm
	BSF	GPIO, SERVO_PIN

	; delay for high
	MOVFW	servo_period
	MOVWF	delay_counter
	CALL	DELAY	
	
	; set low of pwm
	BCF	GPIO, SERVO_PIN

	; delay for low
	MOVLW	SERVO_TOTAL
	MOVWF	delay_counter
	MOVFW	servo_period
	SUBWF	delay_counter, f
	CALL	DELAY

	GOTO	MAIN_LOOP
	

DELAY
	MOVLW	D'31'
	MOVWF	delay_counter_inner
	NOP
	NOP
	NOP
DELAY_LOOP
	DECFSZ	delay_counter_inner, f
	GOTO DELAY_LOOP
	DECFSZ	delay_counter, f
	GOTO DELAY

	RETURN

	
	END