
;understanding subtraction
;
;subtracting data byte with the A
;
;SUI <data>
; Subtract Imediate

jmp start

;data


;code
start:	MVI A, 0ah
	SUI 04h

	OUT 01h

hlt