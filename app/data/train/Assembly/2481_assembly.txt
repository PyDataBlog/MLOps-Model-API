
;Understanding jump instructions
;
; 2-types : conditional, un-conditional
;
; JMP <address|label>
; jumps to specified address or label
;
; P.S. Don't run in gnusim8085, infinite loop
; freezes the program

jmp start

;data


;code
start:	MVI A,00h

begin:	ADI 01h
	OUT 01h

	; 3-byte instruction
	JMP begin

hlt