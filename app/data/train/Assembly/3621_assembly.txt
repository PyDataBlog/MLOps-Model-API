
; Understanding comparisons
;
; CMP R/M compare the contents of memory/
; register with accumulator
; CPI <8 bit data> 
;
; Flags are set to indicate the comparison
; A == Z, Z set and C reset
; A < Z, Z reset and C set
; A > Z, Z reset and C reset

jmp start

;data


;code
start: nop
	MVI C, 00h ; low order byte
	MVI B, 00h ; high order byte
	LXI H, 0000h ; memory pointer

begin:	MOV A, M
	CPI 00h

check_zero:	JNZ add_data
		JNC end

add_data:	ADD C
		MOV C, A
		JC inr_carry

inr_memory:	INX H
		JMP begin

end: hlt 

inr_carry:	INR B
		JMP inr_memory

hlt