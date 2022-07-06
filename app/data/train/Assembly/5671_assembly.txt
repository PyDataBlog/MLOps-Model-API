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
; glb modoAp_t : unsigned short
; glb tramaDWords_t : struct <something>
; glb tramaWords_t : struct <something>
; RPN'ized expression: "2 "
; Expanded expression: "2 "
; Expression value: 2
; RPN'ized expression: "2 "
; Expanded expression: "2 "
; Expression value: 2
; RPN'ized expression: "2 "
; Expanded expression: "2 "
; Expression value: 2
; RPN'ized expression: "2 "
; Expanded expression: "2 "
; Expression value: 2
; glb tramaBytes_t : struct <something>
; glb trama_t : union <something>
; RPN'ized expression: "8 "
; Expanded expression: "8 "
; Expression value: 8
; glb bloque_t : struct <something>
; glb ptrBloque_t : * struct <something>
; glb dobleEnlace_t : struct <something>
; glb c2c_t : struct <something>
; glb posicionC2c : (
; prm     i : int
; prm     c2c : struct <something>
;     ) int
; glb eliminarC2c : (
; prm     i : int
; prm     c2c : struct <something>
;     ) void
; glb apilarC2c : (
; prm     i : int
; prm     c2c : struct <something>
;     ) void
; glb encolarC2c : (
; prm     i : int
; prm     c2c : struct <something>
;     ) void
; glb desencolarC2c : (
; prm     c2c : struct <something>
;     ) int
; glb inicializarC2c : (
; prm     c2c : * struct <something>
; prm     e : * struct <something>
; prm     cabecera : int
; prm     compartida : int
;     ) void
; glb ptrC2c_t : * struct <something>
; glb posicionPC2c : (
; prm     i : int
; prm     c2c : * struct <something>
;     ) int
; glb eliminarPC2c : (
; prm     i : int
; prm     ptrC2c : * struct <something>
;     ) void
; glb apilarPC2c : (
; prm     i : int
; prm     ptrC2c : * struct <something>
;     ) void
; glb encolarPC2c : (
; prm     i : int
; prm     ptrC2c : * struct <something>
;     ) void
; glb desencolarPC2c : (
; prm     ptrC2c : * struct <something>
;     ) int
; glb inicializarPC2c : (
; prm     ptrC2c : * struct <something>
; prm     e : * struct <something>
; prm     cabecera : int
; prm     compartida : int
;     ) void
; glb callBack_t : * (
; prm     arg : * void
;     ) int
; RPN'ized expression: "10 "
; Expanded expression: "10 "
; Expression value: 10
; glb descCcb_t : struct <something>
; glb ccb_t : * struct <something>
; glb inicCcb : (
; prm     ccb : * struct <something>
; prm     max : unsigned short
;     ) int
; glb encolarCcb : (
; prm     cb : * (
; prm         arg : * void
;         ) int
; prm     ccb : * struct <something>
;     ) int
; glb desencolarCcb : (
; prm     ccb : * struct <something>
;     ) * (
; prm     arg : * void
;     ) int
; glb eliminarCcb : (
; prm     cb : * (
; prm         arg : * void
;         ) int
; prm     ccb : * struct <something>
;     ) int
; glb eliminarSegCcb : (
; prm     segmento : unsigned short
; prm     ccb : * struct <something>
;     ) int
; glb vaciarCcb : (
; prm     ccb : * struct <something>
;     ) int
; glb atenderCcb : (
; prm     ccb : * struct <something>
;     ) int
; glb estado_t : int
; glb dfa_t : struct <something>
; RPN'ized expression: "12 "
; Expanded expression: "12 "
; Expression value: 12
; RPN'ized expression: "80 "
; Expanded expression: "80 "
; Expression value: 80
; RPN'ized expression: "10 "
; Expanded expression: "10 "
; Expression value: 10
; glb descProceso_t : struct <something>
; glb descThread_t : struct <something>
; glb tipoFichero_t : int
; RPN'ized expression: "9 "
; Expanded expression: "9 "
; Expression value: 9
; glb descFichero_t : struct <something>
; glb tipoRecurso_t : int
; glb open_t : * (
; prm     dfs : int
; prm     modo : unsigned short
;     ) int
; glb release_t : * (
; prm     dfs : int
;     ) int
; glb read_t : * (
; prm     dfs : int
; prm     dir : * unsigned char
; prm     nbytes : unsigned short
;     ) int
; glb aio_read_t : * (
; prm     dfs : int
; prm     dir : * unsigned char
; prm     nbytes : unsigned short
;     ) int
; glb write_t : * (
; prm     dfs : int
; prm     dir : * unsigned char
; prm     nbytes : unsigned short
;     ) int
; glb aio_write_t : * (
; prm     dfs : int
; prm     dir : * unsigned char
; prm     nbytes : unsigned short
;     ) int
; glb lseek_t : * (
; prm     dfs : int
; prm     pos : int
; prm     whence : unsigned short
;     ) int
; glb fcntl_t : * (
; prm     dfs : int
; prm     cmd : unsigned short
; prm     arg : unsigned short
;     ) int
; glb ioctl_t : * (
; prm     dfs : int
; prm     request : unsigned short
; prm     arg : unsigned short
;     ) int
; glb eliminar_t : * (
; prm     pindx : int
;     ) int
; RPN'ized expression: "12 "
; Expanded expression: "12 "
; Expression value: 12
; RPN'ized expression: "2 "
; Expanded expression: "2 "
; Expression value: 2
; RPN'ized expression: "2 "
; Expanded expression: "2 "
; Expression value: 2
; RPN'ized expression: "2 "
; Expanded expression: "2 "
; Expression value: 2
; glb descRecurso_t : struct <something>
; glb info_t : struct <something>
; glb cabecera_t : struct <something>
; RPN'ized expression: "16 1 + "
; Expanded expression: "17 "
; Expression value: 17
; RPN'ized expression: "16 2 + "
; Expanded expression: "18 "
; Expression value: 18
; RPN'ized expression: "2010 1 + "
; Expanded expression: "2011 "
; Expression value: 2011
; RPN'ized expression: "2010 2 + "
; Expanded expression: "2012 "
; Expression value: 2012
; RPN'ized expression: "20 1 + "
; Expanded expression: "21 "
; Expression value: 21
; RPN'ized expression: "20 2 + "
; Expanded expression: "22 "
; Expression value: 22
; RPN'ized expression: "14 1 + "
; Expanded expression: "15 "
; Expression value: 15
; RPN'ized expression: "14 2 + "
; Expanded expression: "16 "
; Expression value: 16
; RPN'ized expression: "16 16 + "
; Expanded expression: "32 "
; Expression value: 32
; RPN'ized expression: "2010 16 + "
; Expanded expression: "2026 "
; Expression value: 2026
; RPN'ized expression: "2010 1 + "
; Expanded expression: "2011 "
; Expression value: 2011
; RPN'ized expression: "2010 1 + "
; Expanded expression: "2011 "
; Expression value: 2011
; RPN'ized expression: "16 1 + "
; Expanded expression: "17 "
; Expression value: 17
; RPN'ized expression: "2010 1 + "
; Expanded expression: "2011 "
; Expression value: 2011
; RPN'ized expression: "20 14 + "
; Expanded expression: "34 "
; Expression value: 34
; glb e2PFR_t : struct <something>
; glb cPFR_t : int
; glb sigThread_t : * () int
; glb activarThread_t : * (
; prm     tindx : int
;     ) void
; glb buscarNuevoThreadActual_t : * (void) void
; glb bloquearThreadActual_t : * (
; prm     rindx : int
;     ) void
; glb descSO1H_t : struct <something>
; glb thread_attribs_t : struct <something>
; glb thread_create : (
; prm     tid : * int
; prm     attribs : * struct <something>
; prm     funcionInicial : * * void
; prm     arg : * void
;     ) int
; glb thread_join : (
; prm     tid : int
; prm     res : * * void
;     ) int
; glb thread_exit : (
; prm     res : * void
;     ) void
; glb thread_yield : (void) int
; glb thread_self : (void) int
; glb pos_t : struct <something>
; RPN'ized expression: "50 "
; Expanded expression: "50 "
; Expression value: 50
; RPN'ized expression: "80 "
; Expanded expression: "80 "
; Expression value: 80
; glb pantalla_t : union <something>
; glb tamPantalla : (
; prm     numFilas : unsigned char
;     ) unsigned short
; glb pantallazo : (
; prm     pantalla : * union <something>
; prm     numFilas : unsigned short
; prm     car : char
; prm     atr : char
; prm     esqSupDeF : unsigned char
; prm     esqSupDeC : unsigned char
; prm     esqInfIzF : unsigned char
; prm     esqInfIzC : unsigned char
;     ) void
; glb borrarPantalla : (
; prm     pantalla : * union <something>
; prm     numFilas : unsigned short
;     ) void
; glb borrarCPantalla : (
; prm     pantalla : * union <something>
; prm     numFilas : unsigned short
;     ) void
; glb copiarPantalla : (
; prm     p1 : * union <something>
; prm     p2 : * union <something>
; prm     numFilas : unsigned short
;     ) void
; glb copiarCPantalla : (
; prm     p1 : * union <something>
; prm     p2 : * union <something>
; prm     numFilas : unsigned short
;     ) void
; glb scrollPantalla : (
; prm     pantalla : * union <something>
; prm     numFilas : unsigned short
;     ) void
; glb scrollCPantalla : (
; prm     pantalla : * union <something>
; prm     numFilas : unsigned short
;     ) void
; glb cursorF : unsigned char
; glb cursorC : unsigned char
; glb lineaOriginal1 : unsigned char
; glb lineaOriginal2 : unsigned char
; glb inicMemVideo : (void) void
; glb finMemVideo : (void) int
; glb goToXYVideo : (
; prm     F : unsigned char
; prm     C : unsigned char
;     ) void
; glb printCarVideo : (
; prm     car : char
;     ) int
; glb printCarVideo : (
; prm     car : char
;     ) int
; glb printLnVideo : (void) int
; glb printStrVideo : (
; prm     str : * char
;     ) int
; glb printStrHastaVideo : (
; prm     str : * char
; prm     n : unsigned short
; prm     lleno : int
;     ) int
; glb printDecVideo : (
; prm     num : unsigned short
; prm     l : unsigned short
;     ) int
; glb printLDecVideo : (
; prm     num : unsigned
; prm     l : unsigned short
;     ) int
; glb printIntVideo : (
; prm     num : int
; prm     l : unsigned short
;     ) int
; glb printLIntVideo : (
; prm     num : int
; prm     l : unsigned short
;     ) int
; glb printHexVideo : (
; prm     num : unsigned short
; prm     l : unsigned short
;     ) int
; glb printLHexVideo : (
; prm     num : unsigned
; prm     l : unsigned short
;     ) int
; glb printBinVideo : (
; prm     num : unsigned short
; prm     l : unsigned short
;     ) int
; glb printLBinVideo : (
; prm     num : unsigned
; prm     l : unsigned short
;     ) int
; glb printPtrVideo : (
; prm     ptr : * unsigned char
;     ) int
; glb printByteVideo : (
; prm     b : unsigned char
;     ) int
; glb printWordVideo : (
; prm     w : unsigned short
;     ) int
; glb printCadVideo : (
; prm     cad : * char
;     ) int
; glb HWLIST_t : unsigned short
; glb KBDFLAGS_t : unsigned short
; glb KBDSTATUS_t : unsigned short
; glb VIDEOCONTROL_t : unsigned char
; glb DRECSTATUS_t : unsigned char
; glb DMOTORSTATUS_t : unsigned char
; glb FDOPSTATUS_t : unsigned char
; glb VIDEOSWITCHES_t : unsigned char
; glb VGAOPTIONS_t : unsigned char
; glb VIDEOSAVETBL_t : struct <something>
; RPN'ized expression: "4 "
; Expanded expression: "4 "
; Expression value: 4
; RPN'ized expression: "25 "
; Expanded expression: "25 "
; Expression value: 25
; RPN'ized expression: "20 "
; Expanded expression: "20 "
; Expression value: 20
; RPN'ized expression: "9 "
; Expanded expression: "9 "
; Expression value: 9
; glb VIDEOPARAM_t : struct <something>
; RPN'ized expression: "256 "
; Expanded expression: "256 "
; Expression value: 256
; RPN'ized expression: "4 "
; Expanded expression: "4 "
; Expression value: 4
; RPN'ized expression: "3 "
; Expanded expression: "3 "
; Expression value: 3
; RPN'ized expression: "16 "
; Expanded expression: "16 "
; Expression value: 16
; RPN'ized expression: "7 "
; Expanded expression: "7 "
; Expression value: 7
; RPN'ized expression: "8 "
; Expanded expression: "8 "
; Expression value: 8
; RPN'ized expression: "2 "
; Expanded expression: "2 "
; Expression value: 2
; RPN'ized expression: "2 "
; Expanded expression: "2 "
; Expression value: 2
; RPN'ized expression: "5 "
; Expanded expression: "5 "
; Expression value: 5
; RPN'ized expression: "2 "
; Expanded expression: "2 "
; Expression value: 2
; RPN'ized expression: "3 "
; Expanded expression: "3 "
; Expression value: 3
; RPN'ized expression: "4 "
; Expanded expression: "4 "
; Expression value: 4
; RPN'ized expression: "11 "
; Expanded expression: "11 "
; Expression value: 11
; RPN'ized expression: "7 "
; Expanded expression: "7 "
; Expression value: 7
; RPN'ized expression: "68 "
; Expanded expression: "68 "
; Expression value: 68
; RPN'ized expression: "16 "
; Expanded expression: "16 "
; Expression value: 16
; glb BIOSAREA_t : struct <something>
; glb ptrBiosArea : * struct <something>
; glb ptrFechaBios : * unsigned char
; glb funcion : (
; prm     arg : * void
;     ) * void
section .text
_funcion:
	push	ebp
	movzx	ebp, sp
	 sub	sp,          8
; loc     arg : (@8): * void
; loc     i : (@-4): int
; loc     j : (@-8): unsigned
; loc     <something> : int
; RPN'ized expression: "i arg (something3) = "
; Expanded expression: "(@-4) (@8) *(4) =(4) "
; Fused expression:    "=(204) *(@-4) *(@8) "
	mov	eax, [bp+8]
	mov	[bp-4], eax
; for
; RPN'ized expression: "j 0 = "
; Expanded expression: "(@-8) 0 =(4) "
; Fused expression:    "=(204) *(@-8) 0 "
	mov	eax, 0
	mov	[bp-8], eax
L4:
; RPN'ized expression: "j 10000 < "
; Expanded expression: "(@-8) *(4) 10000 <u "
; Fused expression:    "<u *(@-8) 10000 IF! "
	mov	eax, [bp-8]
	cmp	eax, 10000
	jae	L7
; RPN'ized expression: "j ++p "
; Expanded expression: "(@-8) ++p(4) "
; {
; loc         <something> : * char
; RPN'ized expression: "753664 2 78 2 i * - * + (something8) *u ++p "
; Expanded expression: "753664 2 78 2 (@-4) *(4) * - * + ++p(-1) "
; Fused expression:    "* 2 *(@-4) - 78 ax * 2 ax + 753664 ax ++p(-1) *ax "
	mov	eax, 2
	mul	dword [bp-4]
	mov	ecx, eax
	mov	eax, 78
	sub	eax, ecx
	mov	ecx, eax
	mov	eax, 2
	mul	ecx
	mov	ecx, eax
	mov	eax, 753664
	add	eax, ecx
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	al, [si]
	movsx	eax, al
	inc	byte [si]
; if
; RPN'ized expression: "j 20 % 19 == "
; Expanded expression: "(@-8) *(4) 20 %u 19 == "
; Fused expression:    "%u *(@-8) 20 == ax 19 IF! "
	mov	eax, [bp-8]
	mov	edx, 0
	mov	ecx, 20
	div	ecx
	mov	eax, edx
	cmp	eax, 19
	jne	L9
; RPN'ized expression: "( thread_yield ) "
; Expanded expression: " thread_yield ()0 "
; Fused expression:    "( thread_yield )0 "
	db	0x9A
section .relot
	dd	L11
section .text
L11:
	dd	_thread_yield
L9:
; }
L5:
; Fused expression:    "++p(4) *(@-8) "
	mov	eax, [bp-8]
	inc	dword [bp-8]
	jmp	L4
L7:
; loc     <something> : * void
; RPN'ized expression: "( i (something12) thread_exit ) "
; Expanded expression: " (@-4) *(4)  thread_exit ()4 "
; Fused expression:    "( *(4) (@-4) , thread_exit )4 "
	push	dword [bp-4]
	db	0x9A
section .relot
	dd	L13
section .text
L13:
	dd	_thread_exit
	sub	sp, -4
L1:
	db	0x66
	leave
	retf
L14:

section .fxnsz noalloc
	dd	L14 - _funcion

; glb retardarTics : (
; prm     tics : unsigned
;     ) void
section .text
_retardarTics:
	push	ebp
	movzx	ebp, sp
	 sub	sp,          4
; loc     tics : (@8): unsigned
; loc     tics_0 : (@-4): unsigned
; RPN'ized expression: "tics_0 ptrBiosArea BIOS_timer -> *u = "
; Expanded expression: "(@-4) ptrBiosArea *(4) 1132 + *(4) =(4) "
; Fused expression:    "ptrBiosArea + *ax 1132 =(204) *(@-4) *ax "
section .relod
	dd	L17
section .text
	db	0x66, 0xB8
L17:
	dd	_ptrBiosArea
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 1132
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	mov	[bp-4], eax
; while
; RPN'ized expression: "ptrBiosArea BIOS_timer -> *u tics_0 tics + <= "
; Expanded expression: "ptrBiosArea *(4) 1132 + *(4) (@-4) *(4) (@8) *(4) + <=u "
L18:
; Fused expression:    "ptrBiosArea + *ax 1132 push-ax + *(@-4) *(@8) <=u **sp ax IF! "
section .relod
	dd	L20
section .text
	db	0x66, 0xB8
L20:
	dd	_ptrBiosArea
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 1132
	push	eax
	mov	eax, [bp-4]
	add	eax, [bp+8]
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	cmp	eax, ecx
	ja	L19
; {
; }
	jmp	L18
L19:
L15:
	db	0x66
	leave
	retf
L21:

section .fxnsz
	dd	L21 - _retardarTics

; glb demo_2 : (void) void
section .text
	global	_demo_2
_demo_2:
	push	ebp
	movzx	ebp, sp
	 sub	sp,         20
sti
; RPN'ized expression: "( 12 printCarVideo ) "
; Expanded expression: " 12  printCarVideo ()4 "
; Fused expression:    "( 12 , printCarVideo )4 "
	push	dword 12
	db	0x9A
section .relot
	dd	L24
section .text
L24:
	dd	_printCarVideo
	sub	sp, -4
; for
; loc     i : (@-4): int
; RPN'ized expression: "i 20 = "
; Expanded expression: "(@-4) 20 =(4) "
; Fused expression:    "=(204) *(@-4) 20 "
	mov	eax, 20
	mov	[bp-4], eax
L25:
; RPN'ized expression: "i 60 < "
; Expanded expression: "(@-4) *(4) 60 < "
; Fused expression:    "< *(@-4) 60 IF! "
	mov	eax, [bp-4]
	cmp	eax, 60
	jge	L28
; RPN'ized expression: "i ++p "
; Expanded expression: "(@-4) ++p(4) "
; {
; loc         <something> : * union <something>
; loc         <something> : char
; RPN'ized expression: "( i , 21 , 20 , 21 , 7 , 254 (something30) , 50 , 753664 (something29) pantallazo ) "
; Expanded expression: " (@-4) *(4)  21  20  21  7  -2  50  753664  pantallazo ()32 "
; Fused expression:    "( *(4) (@-4) , 21 , 20 , 21 , 7 , -2 , 50 , 753664 , pantallazo )32 "
	push	dword [bp-4]
	push	dword 21
	push	dword 20
	push	dword 21
	push	dword 7
	push	dword -2
	push	dword 50
	push	dword 753664
	db	0x9A
section .relot
	dd	L31
section .text
L31:
	dd	_pantallazo
	sub	sp, -32
; RPN'ized expression: "( 0 retardarTics ) "
; Expanded expression: " 0  retardarTics ()4 "
; Fused expression:    "( 0 , retardarTics )4 "
	push	dword 0
	db	0x9A
section .relot
	dd	L32
section .text
L32:
	dd	_retardarTics
	sub	sp, -4
; }
L26:
; Fused expression:    "++p(4) *(@-4) "
	mov	eax, [bp-4]
	inc	dword [bp-4]
	jmp	L25
L28:
; for
; loc     i : (@-4): int
; RPN'ized expression: "i 0 = "
; Expanded expression: "(@-4) 0 =(4) "
; Fused expression:    "=(204) *(@-4) 0 "
	mov	eax, 0
	mov	[bp-4], eax
L33:
; RPN'ized expression: "i 27 < "
; Expanded expression: "(@-4) *(4) 27 < "
; Fused expression:    "< *(@-4) 27 IF! "
	mov	eax, [bp-4]
	cmp	eax, 27
	jge	L36
; RPN'ized expression: "i ++p "
; Expanded expression: "(@-4) ++p(4) "
; {
; loc         tid : (@-8): int

section .data
	align 4
L37:
; RPN'ized expression: "16384 "
; Expanded expression: "16384 "
; Expression value: 16384
	dw	16384
; RPN'ized expression: "0 "
; Expanded expression: "0 "
; Expression value: 0
	dw	0

section .text
; loc         attribs : (@-12): struct <something>
; =
; Fused expression:    "( (@-12) , L37 , 4u , L38 )12 "
	xor	eax, eax
	mov	ax, ss
	shl	eax, 4
	lea	eax, [ebp+eax-12]
	push	eax
section .relod
	dd	L39
section .text
	db	0x66, 0x68
L39:
	dd	L37
	push	dword 4
	db	0x9A
section .relot
	dd	L40
section .text
L40:
	dd	L38
	sub	sp, -12
; RPN'ized expression: "( i , funcion , attribs &u , tid &u thread_create ) "
; Expanded expression: " (@-4) *(4)  funcion  (@-12)  (@-8)  thread_create ()16 "
; Fused expression:    "( *(4) (@-4) , funcion , (@-12) , (@-8) , thread_create )16 "
	push	dword [bp-4]
section .relod
	dd	L41
section .text
	db	0x66, 0x68
L41:
	dd	_funcion
	xor	eax, eax
	mov	ax, ss
	shl	eax, 4
	lea	eax, [ebp+eax-12]
	push	eax
	xor	eax, eax
	mov	ax, ss
	shl	eax, 4
	lea	eax, [ebp+eax-8]
	push	eax
	db	0x9A
section .relot
	dd	L42
section .text
L42:
	dd	_thread_create
	sub	sp, -16
; }
L34:
; Fused expression:    "++p(4) *(@-4) "
	mov	eax, [bp-4]
	inc	dword [bp-4]
	jmp	L33
L36:
; for
; loc     i : (@-4): int
; RPN'ized expression: "i 1 = "
; Expanded expression: "(@-4) 1 =(4) "
; Fused expression:    "=(204) *(@-4) 1 "
	mov	eax, 1
	mov	[bp-4], eax
L43:
; RPN'ized expression: "i 27 1 + < "
; Expanded expression: "(@-4) *(4) 28 < "
; Fused expression:    "< *(@-4) 28 IF! "
	mov	eax, [bp-4]
	cmp	eax, 28
	jge	L46
; RPN'ized expression: "i ++p "
; Expanded expression: "(@-4) ++p(4) "
; {
; loc         tid0 : (@-8): int
; loc         tid : (@-12): int
; loc         ptrVoid : (@-16): * void
; loc         res : (@-20): * * void
; RPN'ized expression: "tid0 27 i - 1 + = "
; Expanded expression: "(@-8) 27 (@-4) *(4) - 1 + =(4) "
; Fused expression:    "- 27 *(@-4) + ax 1 =(204) *(@-8) ax "
	mov	eax, 27
	sub	eax, [bp-4]
	inc	eax
	mov	[bp-8], eax
; loc         <something> : * * void
; RPN'ized expression: "res ptrVoid &u (something47) = "
; Expanded expression: "(@-20) (@-16) =(4) "
; Fused expression:    "=(204) *(@-20) (@-16) "
	xor	eax, eax
	mov	ax, ss
	shl	eax, 4
	lea	eax, [ebp+eax-16]
	mov	[bp-20], eax
; RPN'ized expression: "tid ( res , tid0 thread_join ) = "
; Expanded expression: "(@-12)  (@-20) *(4)  (@-8) *(4)  thread_join ()8 =(4) "
; Fused expression:    "( *(4) (@-20) , *(4) (@-8) , thread_join )8 =(204) *(@-12) ax "
	push	dword [bp-20]
	push	dword [bp-8]
	db	0x9A
section .relot
	dd	L48
section .text
L48:
	dd	_thread_join
	sub	sp, -8
	mov	[bp-12], eax
; }
L44:
; Fused expression:    "++p(4) *(@-4) "
	mov	eax, [bp-4]
	inc	dword [bp-4]
	jmp	L43
L46:
cli

section .rodata
L49:
	db	10," Pulse una tecla para continuar ... "
	times	1 db 0

section .text
; RPN'ized expression: "( L49 printStrVideo ) "
; Expanded expression: " L49  printStrVideo ()4 "
; Fused expression:    "( L49 , printStrVideo )4 "
section .relod
	dd	L50
section .text
	db	0x66, 0x68
L50:
	dd	L49
	db	0x9A
section .relot
	dd	L51
section .text
L51:
	dd	_printStrVideo
	sub	sp, -4
L22:
	db	0x66
	leave
	retf
L52:

section .fxnsz
	dd	L52 - _demo_2

section .text
L38:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
	mov	edi, [bp+16]
	ror	edi, 4
	mov	es, di
	shr	edi, 28
	mov	esi, [bp+12]
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ebx, [bp+8]
	cld
L53:
	mov	ecx, 32768
	cmp	ebx, ecx
	jc	L54
	sub	ebx, ecx
	rep	movsb
	and	di, 15
	mov	ax, es
	add	ax, 2048
	mov	es, ax
	and	si, 15
	mov	ax, ds
	add	ax, 2048
	mov	ds, ax
	jmp	L53
L54:
	mov	cx, bx
	rep	movsb
	mov	eax, [bp+16]
	db	0x66
	leave
	retf


	extern	_thread_yield
	extern	_thread_exit
	extern	_ptrBiosArea
	extern	_printCarVideo
	extern	_pantallazo
	extern	_thread_create
	extern	_thread_join
	extern	_printStrVideo

; Syntax/declaration table/stack:
; Bytes used: 11530/40960


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
; Ident modoAp_t
; Ident DS
; Ident ES
; Ident EDI
; Ident ESI
; Ident EBP
; Ident ESP
; Ident EBX
; Ident EDX
; Ident ECX
; Ident EAX
; Ident IP
; Ident CS
; Ident Flags
; Ident tramaDWords_t
; Ident DI
; Ident rDI
; Ident SI
; Ident rSI
; Ident BP
; Ident rBP
; Ident SP
; Ident rSP
; Ident BX
; Ident rBX
; Ident DX
; Ident rDX
; Ident CX
; Ident rCX
; Ident AX
; Ident rAX
; Ident tramaWords_t
; Ident BL
; Ident BH
; Ident rB
; Ident DL
; Ident DH
; Ident rD
; Ident CL
; Ident CH
; Ident rC
; Ident AL
; Ident AH
; Ident rA
; Ident tramaBytes_t
; Ident td
; Ident tw
; Ident tb
; Ident trama_t
; Ident tam
; Ident sig
; Ident ant
; Ident aux
; Ident relleno
; Ident bloque_t
; Ident ptrBloque_t
; Ident cab
; Ident dobleEnlace_t
; Ident numElem
; Ident primero
; Ident cabecera
; Ident e
; Ident c2c_t
; Ident posicionC2c
; Ident i
; Ident c2c
; Ident eliminarC2c
; Ident apilarC2c
; Ident encolarC2c
; Ident desencolarC2c
; Ident inicializarC2c
; Ident compartida
; Ident ptrC2c_t
; Ident posicionPC2c
; Ident eliminarPC2c
; Ident ptrC2c
; Ident apilarPC2c
; Ident encolarPC2c
; Ident desencolarPC2c
; Ident inicializarPC2c
; Ident callBack_t
; Ident arg
; Ident num
; Ident in
; Ident out
; Ident max
; Ident callBack
; Ident descCcb_t
; Ident ccb_t
; Ident inicCcb
; Ident ccb
; Ident encolarCcb
; Ident cb
; Ident desencolarCcb
; Ident eliminarCcb
; Ident eliminarSegCcb
; Ident segmento
; Ident vaciarCcb
; Ident atenderCcb
; Ident libre
; Ident preparado
; Ident ejecutandose
; Ident bloqueado
; Ident estado_t
; Ident modoAp
; Ident dfs
; Ident pos
; Ident dfa_t
; Ident pid
; Ident noStatus
; Ident status
; Ident ppindx
; Ident hpindx
; Ident c2cHijos
; Ident c2cThreads
; Ident CSProc
; Ident tamCodigo
; Ident desplBSS
; Ident desplPila
; Ident tamFichero
; Ident programa
; Ident comando
; Ident nfa
; Ident tfa
; Ident uid
; Ident gid
; Ident descProceso_t
; Ident tid
; Ident estado
; Ident esperandoPor
; Ident trama
; Ident ptindx
; Ident htindx
; Ident pindx
; Ident SSThread
; Ident SP0
; Ident descThread_t
; Ident flibre
; Ident fRegular
; Ident fedBloques
; Ident fedCaracteres
; Ident tuberia
; Ident tipoFichero_t
; Ident tipo
; Ident nombre
; Ident rindx
; Ident menor
; Ident shareMode
; Ident contAp_L
; Ident contAp_E
; Ident descFichero_t
; Ident rLibre
; Ident rDCaracteres
; Ident rDBloques
; Ident rTuberia
; Ident rGP
; Ident rGM
; Ident rSF
; Ident rOtro
; Ident tipoRecurso_t
; Ident open_t
; Ident modo
; Ident release_t
; Ident read_t
; Ident dir
; Ident nbytes
; Ident aio_read_t
; Ident write_t
; Ident aio_write_t
; Ident lseek_t
; Ident whence
; Ident fcntl_t
; Ident cmd
; Ident ioctl_t
; Ident request
; Ident eliminar_t
; Ident tindx
; Ident c2cFichRec
; Ident numVI
; Ident nVInt
; Ident irq
; Ident isr
; Ident open
; Ident release
; Ident read
; Ident aio_read
; Ident write
; Ident aio_write
; Ident lseek
; Ident fcntl
; Ident ioctl
; Ident eliminar
; Ident descRecurso_t
; Ident SP0_So1
; Ident IMR
; Ident modoSO1
; Ident ptrDebugWord
; Ident info_t
; Ident signatura
; Ident bytesUltSector
; Ident sectores
; Ident numDirReub
; Ident numParCabecera
; Ident minAlloc
; Ident maxAlloc
; Ident SS0
; Ident checkSum
; Ident IP0
; Ident CS0
; Ident offTablaReub
; Ident numOverlay
; Ident cabecera_t
; Ident Libres
; Ident Ocupados
; Ident e2DescProceso
; Ident e2DescThread
; Ident e2DescFichero
; Ident e2DescRecurso
; Ident e2Hijos
; Ident e2Threads
; Ident e2Preparados
; Ident e2Urgentes
; Ident e2POrdenados
; Ident e2TDormidos
; Ident e2FichRec
; Ident e2PFR_t
; Ident DPLibres
; Ident DPOcupados
; Ident DTLibres
; Ident DTOcupados
; Ident TPreparados
; Ident TUrgentes
; Ident POrdenados
; Ident TDormidos
; Ident DFLibres
; Ident DFOcupados
; Ident DRLibres
; Ident DROcupados
; Ident numColasPFR
; Ident cPFR_t
; Ident sigThread_t
; Ident activarThread_t
; Ident buscarNuevoThreadActual_t
; Ident bloquearThreadActual_t
; Ident ptrIndProcesoActual
; Ident ptrIndThreadActual
; Ident ptrTramaThread
; Ident ptrActivarAlEpilogo
; Ident ptrDescProceso
; Ident tamDescProceso
; Ident ptrDescThread
; Ident tamDescThread
; Ident ptrDescFichero
; Ident ptrDescRecurso
; Ident ptrC2cPFR
; Ident ptrE2PFR
; Ident ptrNivelActivacionSO1H
; Ident ptrEnHalt
; Ident ptrHayTic
; Ident ptrCcbAlEpilogo
; Ident ptrSS_Thread
; Ident ptrSP_Thread
; Ident ptrSS_Kernel
; Ident ptrSP0_Kernel
; Ident SP0_SO1H
; Ident ptrContRodajas
; Ident ptrContTicsRodaja
; Ident ptrVIOrg
; Ident sigThread
; Ident activarThread
; Ident buscarNuevoThreadActual
; Ident bloquearThreadActual
; Ident ptrListaLibres
; Ident ptrTamBloqueMax
; Ident descSO1H_t
; Ident opciones
; Ident thread_attribs_t
; Ident thread_create
; Ident attribs
; Ident funcionInicial
; Ident thread_join
; Ident res
; Ident thread_exit
; Ident thread_yield
; Ident thread_self
; Ident car
; Ident atr
; Ident pos_t
; Ident t
; Ident pantalla_t
; Ident tamPantalla
; Ident numFilas
; Ident pantallazo
; Ident pantalla
; Ident esqSupDeF
; Ident esqSupDeC
; Ident esqInfIzF
; Ident esqInfIzC
; Ident borrarPantalla
; Ident borrarCPantalla
; Ident copiarPantalla
; Ident p1
; Ident p2
; Ident copiarCPantalla
; Ident scrollPantalla
; Ident scrollCPantalla
; Ident cursorF
; Ident cursorC
; Ident lineaOriginal1
; Ident lineaOriginal2
; Ident inicMemVideo
; Ident finMemVideo
; Ident goToXYVideo
; Ident F
; Ident C
; Ident printCarVideo
; Ident printLnVideo
; Ident printStrVideo
; Ident str
; Ident printStrHastaVideo
; Ident n
; Ident lleno
; Ident printDecVideo
; Ident l
; Ident printLDecVideo
; Ident printIntVideo
; Ident printLIntVideo
; Ident printHexVideo
; Ident printLHexVideo
; Ident printBinVideo
; Ident printLBinVideo
; Ident printPtrVideo
; Ident printByteVideo
; Ident b
; Ident printWordVideo
; Ident w
; Ident printCadVideo
; Ident cad
; Ident HWLIST_t
; Ident KBDFLAGS_t
; Ident KBDSTATUS_t
; Ident VIDEOCONTROL_t
; Ident DRECSTATUS_t
; Ident DMOTORSTATUS_t
; Ident FDOPSTATUS_t
; Ident VIDEOSWITCHES_t
; Ident VGAOPTIONS_t
; Ident VIDEO_paramtbl
; Ident VIDEO_dynsavearea
; Ident VIDEO_text_charset
; Ident VIDEO_graph_charset
; Ident VIDEO_savetbl2
; Ident VIDEOSAVETBL_t
; Ident VPARAM_width
; Ident VPARAM_height
; Ident VPARAM_hchar
; Ident VPARAM_pagesize
; Ident VPARAM_SEQC
; Ident VPARAM_MISC
; Ident VPARAM_CRTC
; Ident VPARAM_ATC
; Ident VPARAM_GRC
; Ident VIDEOPARAM_t
; Ident IVT
; Ident COM_base
; Ident LPT_base
; Ident XBDA_seg
; Ident HW_list
; Ident relleno1
; Ident basemem_K
; Ident relleno2
; Ident KBD_flags
; Ident relleno3
; Ident KBD_bufhead
; Ident KBD_buftail
; Ident KBD_buffer
; Ident dRecStatus
; Ident dMotorStatus
; Ident motorShutoffCounter
; Ident fdOpStatus
; Ident relleno4
; Ident VIDEO_mode
; Ident VIDEO_width
; Ident VIDEO_pagesize
; Ident VIDEO_pageoff
; Ident CURSOR_pos
; Ident CURSOR_shape
; Ident VIDEO_pagActiva
; Ident CRTC_base
; Ident CRT_MODE
; Ident CRT_PALETTE
; Ident relleno5
; Ident BIOS_timer
; Ident BIOS_overflow
; Ident BIOS_ctrlbreak
; Ident BIOS_POSTreset
; Ident relleno6
; Ident BIOS_HD_cnt
; Ident relleno7
; Ident LPT_timeout
; Ident BIOS_4B_flags
; Ident COM_timeout
; Ident KBD_start
; Ident KBD_end1
; Ident VIDEO_lastrow
; Ident VIDEO_hchar
; Ident VIDEO_control
; Ident VIDEO_switches
; Ident VGA_options
; Ident VGA_DCC_index
; Ident relleno8
; Ident KBD_status
; Ident TIMER_waitflag
; Ident TIMER_waitmks
; Ident TIMER_active
; Ident relleno9
; Ident VIDEO_ptrtable
; Ident relleno10
; Ident IAC
; Ident BIOSAREA_t
; Ident ptrBiosArea
; Ident ptrFechaBios
; Ident funcion
; Ident retardarTics
; Ident tics
; Ident demo_2
; Bytes used: 5251/16384

; Next label number: 55
; Compilation succeeded.
