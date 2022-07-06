
; Understanding arithmetic operations from 
; memory
;
; ADD M
; adding A with contents of memory address
; present in HL
;
; SUB M
; subtracting A with contents of memory address
; present in HL
;
; INR M
; increment contnets of memory address in HL
;
; DCR M
; decrement contents of memory address in HL

jmp start

;data


;code
start: nop
	LXI H, 0000h
	MVI A, 65h
	INR M

	ADD M

	INX H
	SUB M
	DCR M

	MOV A, M
	DCX H

	ADD M

hlt