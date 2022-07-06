bits 16

; glb intptr_t : int
; glb uintptr_t : unsigned
; glb intmax_t : int
; glb uintmax_t : unsigned
; glb int8_t : signed char
; glb int_least8_t : signed char
; glb int_fast8_t : signed char
; glb uint8_t : unsigned char
; glb uint_least8_t : unsigned char
; glb uint_fast8_t : unsigned char
; glb int16_t : short
; glb int_least16_t : short
; glb int_fast16_t : short
; glb uint16_t : unsigned short
; glb uint_least16_t : unsigned short
; glb uint_fast16_t : unsigned short
; glb int32_t : int
; glb int_least32_t : int
; glb int_fast32_t : int
; glb uint32_t : unsigned
; glb uint_least32_t : unsigned
; glb uint_fast32_t : unsigned
; glb imaxdiv_t : struct <something>
; glb bool_t : int
; glb pointer_t : * unsigned char
; glb funcion_t : * (
; prm     <something> : * void
;     ) * void
; glb manejador_t : * (void) void
; glb rti_t : * (void) void
; glb isr_t : * (void) void
; glb handler_t : * (void) void
; glb retardarThread_t : * (void) int
; glb ptrTVI_t : * * (void) void
; glb modoSO1_t : int
; glb lh_t : struct <something>
; glb address_t : struct <something>
; glb uPtrAdr_t : union <something>
; glb pid_t : int
; glb tid_t : int
; glb uid_t : int
; glb gid_t : int
; glb pindx_t : int
; glb tindx_t : int
; glb df_t : int
; glb dfs_t : int
; glb rindx_t : int
; glb inportb : (
; prm     port : unsigned short
;     ) unsigned char
; glb inport : (
; prm     port : unsigned short
;     ) unsigned short
; glb outport : (
; prm     port : unsigned short
; prm     val : unsigned short
;     ) void
; glb outportb : (
; prm     port : unsigned short
; prm     val : unsigned char
;     ) void
; glb inportb_r : (
; prm     port : unsigned char
;     ) unsigned char
; glb outportb_r : (
; prm     port : unsigned char
; prm     val : unsigned char
;     ) void
; glb contadorTimer0 : (void) unsigned short
; glb inportb : (
; prm     port : unsigned short
;     ) unsigned char
section .text
	global	_inportb
_inportb:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
; loc     port : (@8): unsigned short
   mov dx,[bp+8] 
   in al,dx      
   xor ah,ah     

L1:
	db	0x66
	leave
	retf
L3:

section .fxnsz noalloc
	dd	L3 - _inportb

; glb inport : (
; prm     port : unsigned short
;     ) unsigned short
section .text
	global	_inport
_inport:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
; loc     port : (@8): unsigned short
   mov dx,[bp+8] 
   in ax,dx      

L4:
	db	0x66
	leave
	retf
L6:

section .fxnsz
	dd	L6 - _inport

; glb outportb : (
; prm     port : unsigned short
; prm     val : unsigned char
;     ) void
section .text
	global	_outportb
_outportb:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
; loc     port : (@8): unsigned short
; loc     val : (@12): unsigned char
   mov dx,[bp+ 8] 
   mov al,[bp+12] 
   out dx,al      

L7:
	db	0x66
	leave
	retf
L9:

section .fxnsz
	dd	L9 - _outportb

; glb outport : (
; prm     port : unsigned short
; prm     val : unsigned short
;     ) void
section .text
	global	_outport
_outport:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
; loc     port : (@8): unsigned short
; loc     val : (@12): unsigned short
   mov dx,[bp+ 8] 
   mov ax,[bp+12] 
   out dx,ax      

L10:
	db	0x66
	leave
	retf
L12:

section .fxnsz
	dd	L12 - _outport

; glb inportb_r : (
; prm     port : unsigned char
;     ) unsigned char
section .text
	global	_inportb_r
_inportb_r:
	push	ebp
	movzx	ebp, sp
	 sub	sp,          4
; loc     port : (@8): unsigned char
; loc     __ret : (@-4): int
; RPN'ized expression: "__ret ( port inportb ) = "
; Expanded expression: "(@-4)  (@8) *(1)  inportb ()4 =(4) "
; Fused expression:    "( *(1) (@8) , inportb )4 =(204) *(@-4) ax "
	mov	al, [bp+8]
	movzx	eax, al
	push	eax
	db	0x9A
section .relot
	dd	L15
section .text
L15:
	dd	_inportb
	sub	sp, -4
	mov	[bp-4], eax
jmp $+2 
 jmp $+2 

; return
; RPN'ized expression: "__ret "
; Expanded expression: "(@-4) *(4) "
; Fused expression:    "*(4) (@-4) unsigned char  "
	mov	eax, [bp-4]
	and	eax, 255
L13:
	db	0x66
	leave
	retf
L16:

section .fxnsz
	dd	L16 - _inportb_r

; glb outportb_r : (
; prm     port : unsigned char
; prm     val : unsigned char
;     ) void
section .text
	global	_outportb_r
_outportb_r:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
; loc     port : (@8): unsigned char
; loc     val : (@12): unsigned char
; RPN'ized expression: "( val , port outportb ) "
; Expanded expression: " (@12) *(1)  (@8) *(1)  outportb ()8 "
; Fused expression:    "( *(1) (@12) , *(1) (@8) , outportb )8 "
	mov	al, [bp+12]
	movzx	eax, al
	push	eax
	mov	al, [bp+8]
	movzx	eax, al
	push	eax
	db	0x9A
section .relot
	dd	L19
section .text
L19:
	dd	_outportb
	sub	sp, -8
jmp $+2 
 jmp $+2 

L17:
	db	0x66
	leave
	retf
L20:

section .fxnsz
	dd	L20 - _outportb_r

; glb contadorTimer0 : (void) unsigned short
section .text
	global	_contadorTimer0
_contadorTimer0:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
   mov al,11010010b 
   out 01000011b,al 
   jmp $+2          
   in al,01000000b  
   mov ah,al        
   jmp $+2          
   in al,01000000b  
   xchg ah,al       

L21:
	db	0x66
	leave
	retf
L23:

section .fxnsz
	dd	L23 - _contadorTimer0



; Syntax/declaration table/stack:
; Bytes used: 1490/40960


; Macro table:
; Macro __SMALLER_C__ = `0x0100`
; Macro __SMALLER_C_32__ = ``
; Macro __HUGE__ = ``
; Macro __SMALLER_C_SCHAR__ = ``
; Bytes used: 74/5120


; Identifier table:
; Ident __floatsisf
; Ident __floatunsisf
; Ident __fixsfsi
; Ident __fixunssfsi
; Ident __addsf3
; Ident __subsf3
; Ident __negsf2
; Ident __mulsf3
; Ident __divsf3
; Ident __lesf2
; Ident __gesf2
; Ident intptr_t
; Ident uintptr_t
; Ident intmax_t
; Ident uintmax_t
; Ident int8_t
; Ident int_least8_t
; Ident int_fast8_t
; Ident uint8_t
; Ident uint_least8_t
; Ident uint_fast8_t
; Ident int16_t
; Ident int_least16_t
; Ident int_fast16_t
; Ident uint16_t
; Ident uint_least16_t
; Ident uint_fast16_t
; Ident int32_t
; Ident int_least32_t
; Ident int_fast32_t
; Ident uint32_t
; Ident uint_least32_t
; Ident uint_fast32_t
; Ident <something>
; Ident quot
; Ident rem
; Ident imaxdiv_t
; Ident FALSE
; Ident TRUE
; Ident bool_t
; Ident pointer_t
; Ident funcion_t
; Ident manejador_t
; Ident rti_t
; Ident isr_t
; Ident handler_t
; Ident retardarThread_t
; Ident ptrTVI_t
; Ident modoSO1_Bin
; Ident modoSO1_Exe
; Ident modoSO1_Bs
; Ident modoSO1_t
; Ident lo
; Ident hi
; Ident lh_t
; Ident offset
; Ident segment
; Ident address_t
; Ident ptr
; Ident adr
; Ident uPtrAdr_t
; Ident pid_t
; Ident tid_t
; Ident uid_t
; Ident gid_t
; Ident pindx_t
; Ident tindx_t
; Ident df_t
; Ident dfs_t
; Ident rindx_t
; Ident inportb
; Ident port
; Ident inport
; Ident outport
; Ident val
; Ident outportb
; Ident inportb_r
; Ident outportb_r
; Ident contadorTimer0
; Bytes used: 804/16384

; Next label number: 24
; Compilation succeeded.
