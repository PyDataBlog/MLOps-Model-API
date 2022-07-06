
; First program. Learning to __copy__ data to a
; register.

; MVI (Move Imediate) to copy data byte to a
; specific register.
; Syntax MVI <register>, <data>
;
; MOV to copy the contents of one register to
; another
; Syntax MOV <dest_register>, <src_register>
jmp start

;data


;code
	; 2-byte instruction. Would require 
	; 2 machine cycles, opcode fetch and
	; memory read for instruction fetch 
	; and a total of 7 T-states.
start:	MVI A,0afh

	; 1-byte instruction. Would require 1
	; machine cycle of opcode fetch and a 
	; total of 4 T-states
	MOV B,A

	; Same as MOV B,A
	MOV C,A

	MVI D,04h
	MOV E,D

hlt