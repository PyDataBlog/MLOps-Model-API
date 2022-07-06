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
; glb pointer : (
; prm     seg : unsigned short
; prm     off : unsigned short
;     ) * unsigned char
section .text
	global	_pointer
_pointer:
	push	ebp
	movzx	ebp, sp
	 sub	sp,          4
; loc     seg : (@8): unsigned short
; loc     off : (@12): unsigned short
; loc     uPtrAdr : (@-4): union <something>
; RPN'ized expression: "uPtrAdr &u adr -> *u &u segment -> *u seg = "
; Expanded expression: "(@-2) (@8) *(2) =(2) "
; Fused expression:    "=(170) *(@-2) *(@8) "
	mov	ax, [bp+8]
	movzx	eax, ax
	mov	[bp-2], ax
	movzx	eax, ax
; RPN'ized expression: "uPtrAdr &u adr -> *u &u offset -> *u off = "
; Expanded expression: "(@-4) (@12) *(2) =(2) "
; Fused expression:    "=(170) *(@-4) *(@12) "
	mov	ax, [bp+12]
	movzx	eax, ax
	mov	[bp-4], ax
	movzx	eax, ax
; return
; RPN'ized expression: "uPtrAdr &u ptr -> *u "
; Expanded expression: "(@-4) *(4) "
; Fused expression:    "*(4) (@-4)  "
	mov	eax, [bp-4]
L1:
	db	0x66
	leave
	retf
L3:

section .fxnsz noalloc
	dd	L3 - _pointer

; glb seg : (
; prm     p : * unsigned char
;     ) unsigned short
section .text
	global	_seg
_seg:
	push	ebp
	movzx	ebp, sp
	 sub	sp,          4
; loc     p : (@8): * unsigned char
; loc     uPtrAdr : (@-4): union <something>
; RPN'ized expression: "uPtrAdr &u ptr -> *u p = "
; Expanded expression: "(@-4) (@8) *(4) =(4) "
; Fused expression:    "=(204) *(@-4) *(@8) "
	mov	eax, [bp+8]
	mov	[bp-4], eax
; return
; RPN'ized expression: "uPtrAdr &u adr -> *u &u segment -> *u "
; Expanded expression: "(@-2) *(2) "
; Fused expression:    "*(2) (@-2)  "
	mov	ax, [bp-2]
	movzx	eax, ax
L4:
	db	0x66
	leave
	retf
L6:

section .fxnsz
	dd	L6 - _seg

; glb off : (
; prm     p : * unsigned char
;     ) unsigned short
section .text
	global	_off
_off:
	push	ebp
	movzx	ebp, sp
	 sub	sp,          4
; loc     p : (@8): * unsigned char
; loc     uPtrAdr : (@-4): union <something>
; RPN'ized expression: "uPtrAdr &u ptr -> *u p = "
; Expanded expression: "(@-4) (@8) *(4) =(4) "
; Fused expression:    "=(204) *(@-4) *(@8) "
	mov	eax, [bp+8]
	mov	[bp-4], eax
; return
; RPN'ized expression: "uPtrAdr &u adr -> *u &u offset -> *u "
; Expanded expression: "(@-4) *(2) "
; Fused expression:    "*(2) (@-4)  "
	mov	ax, [bp-4]
	movzx	eax, ax
L7:
	db	0x66
	leave
	retf
L9:

section .fxnsz
	dd	L9 - _off

; glb incPtr : (
; prm     p : * * unsigned char
; prm     i : unsigned short
;     ) void
section .text
	global	_incPtr
_incPtr:
	push	ebp
	movzx	ebp, sp
	 sub	sp,          4
; loc     p : (@8): * * unsigned char
; loc     i : (@12): unsigned short
; loc     despl : (@-4): unsigned short
; loc     <something> : * struct <something>
; RPN'ized expression: "despl p (something12) offset -> *u = "
; Expanded expression: "(@-4) (@8) *(4) 0 + *(2) =(2) "
; Fused expression:    "+ *(@8) 0 =(170) *(@-4) *ax "
	mov	eax, [bp+8]
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	mov	[bp-4], ax
	movzx	eax, ax
  mov ax,[bp- 8] 
  add ax,[bp+12] 
  jnc incOk      

; loc     <something> : * struct <something>
; RPN'ized expression: "p (something13) segment -> *u 4096 += "
; Expanded expression: "(@8) *(4) 2 + 4096 +=(2) "
; Fused expression:    "+ *(@8) 2 +=(172) *ax 4096 "
	mov	eax, [bp+8]
	add	eax, 2
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	add	eax, 4096
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
 incOk:          
 mov [bp-8],ax   

; loc     <something> : * struct <something>
; RPN'ized expression: "p (something14) offset -> *u despl = "
; Expanded expression: "(@8) *(4) 0 + (@-4) *(2) =(2) "
; Fused expression:    "+ *(@8) 0 =(170) *ax *(@-4) "
	mov	eax, [bp+8]
	mov	ebx, eax
	mov	ax, [bp-4]
	movzx	eax, ax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
L10:
	db	0x66
	leave
	retf
L15:

section .fxnsz
	dd	L15 - _incPtr



; Syntax/declaration table/stack:
; Bytes used: 1175/40960


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
; Ident pointer
; Ident seg
; Ident off
; Ident p
; Ident incPtr
; Ident i
; Bytes used: 751/16384

; Next label number: 16
; Compilation succeeded.
