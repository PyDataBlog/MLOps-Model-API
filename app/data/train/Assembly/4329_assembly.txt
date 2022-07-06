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
; RPN'ized expression: "16 "
; Expanded expression: "16 "
; Expression value: 16
; glb nombreDF_t : [16u] char
; RPN'ized expression: "40 "
; Expanded expression: "40 "
; Expression value: 40
; glb comandoDF_t : [40u] char
; glb tipoDF_t : int
; glb entradaDF_t : struct <something>
; glb dirInicial : unsigned
; glb dirCargaFichero : unsigned
; glb dirFinal : unsigned
; glb reubicacion_t : struct <something>
; glb reubicacion : * struct <something>
; glb numER : unsigned
; glb ptrIndProcesoActual : * int
; glb ptrIndThreadActual : * int
; glb ptrTramaThread : * * union <something>
; glb ptrActivarAlEpilogo : * int
; glb ptrDescProceso : * struct <something>
; glb tamDescProceso : unsigned short
; glb ptrDescThread : * struct <something>
; glb tamDescThread : unsigned short
; glb ptrDescFichero : * struct <something>
; glb ptrDescRecurso : * struct <something>
; glb ptrC2cPFR : * struct <something>
; glb ptrE2PFR : * struct <something>
; glb ptrNivelActivacionSO1H : * int
; glb ptrEnHalt : * int
; glb ptrHayTic : * int
; glb ptrCcbAlEpilogo : * * struct <something>
; glb ptrSS_Thread : * unsigned short
; glb ptrSP_Thread : * unsigned short
; glb ptrSS_Kernel : * unsigned short
; glb ptrSP0_Kernel : * unsigned short
; glb SP0_SO1H : unsigned short
; glb ptrContRodajas : * unsigned
; glb ptrContTicsRodaja : * int
; glb ptrVIOrg : * * (void) void
; glb sigThread : * () int
; glb activarThread : * (
; prm     tindx : int
;     ) void
; glb buscarNuevoThreadActual : * (void) void
; glb bloquearThreadActual : * (
; prm     rindx : int
;     ) void
; glb ptrListaLibres : * * struct <something>
; glb ptrTamBloqueMax : * unsigned short
; glb inicProcesos_x : (void) void
; glb size_t : unsigned
; glb memset : (
; prm     <something> : * void
; prm     <something> : int
; prm     <something> : unsigned
;     ) * void
; glb memcpy : (
; prm     <something> : * void
; prm     <something> : * void
; prm     <something> : unsigned
;     ) * void
; glb memmove : (
; prm     <something> : * void
; prm     <something> : * void
; prm     <something> : unsigned
;     ) * void
; glb memchr : (
; prm     <something> : * char
; prm     <something> : int
; prm     <something> : unsigned
;     ) * char
; glb memcmp : (
; prm     <something> : * void
; prm     <something> : * void
; prm     <something> : unsigned
;     ) int
; glb strcpy : (
; prm     <something> : * char
; prm     <something> : * char
;     ) * char
; glb strncpy : (
; prm     <something> : * char
; prm     <something> : * char
; prm     <something> : unsigned
;     ) * char
; glb strxfrm : (
; prm     <something> : * char
; prm     <something> : * char
; prm     <something> : unsigned
;     ) unsigned
; glb strcat : (
; prm     <something> : * char
; prm     <something> : * char
;     ) * char
; glb strncat : (
; prm     <something> : * char
; prm     <something> : * char
; prm     <something> : unsigned
;     ) * char
; glb strlen : (
; prm     <something> : * char
;     ) unsigned
; glb strchr : (
; prm     <something> : * char
; prm     <something> : int
;     ) * char
; glb strrchr : (
; prm     <something> : * char
; prm     <something> : int
;     ) * char
; glb strstr : (
; prm     <something> : * char
; prm     <something> : * char
;     ) * char
; glb strspn : (
; prm     <something> : * char
; prm     <something> : * char
;     ) unsigned
; glb strcspn : (
; prm     <something> : * char
; prm     <something> : * char
;     ) unsigned
; glb strpbrk : (
; prm     <something> : * char
; prm     <something> : * char
;     ) * char
; glb strtok : (
; prm     <something> : * char
; prm     <something> : * char
;     ) * char
; glb strcmp : (
; prm     <something> : * char
; prm     <something> : * char
;     ) int
; glb strncmp : (
; prm     <something> : * char
; prm     <something> : * char
; prm     <something> : unsigned
;     ) int
; glb strcoll : (
; prm     <something> : * char
; prm     <something> : * char
;     ) int
; glb strerror : (
; prm     <something> : int
;     ) * char
; glb ptrIndProcesoActual : * int
section .bss
	alignb 4
	global	_ptrIndProcesoActual
_ptrIndProcesoActual:
	resb	4

; glb ptrIndThreadActual : * int
section .bss
	alignb 4
	global	_ptrIndThreadActual
_ptrIndThreadActual:
	resb	4

; glb ptrTramaThread : * * union <something>
section .bss
	alignb 4
	global	_ptrTramaThread
_ptrTramaThread:
	resb	4

; glb ptrActivarAlEpilogo : * int
section .bss
	alignb 4
	global	_ptrActivarAlEpilogo
_ptrActivarAlEpilogo:
	resb	4

; glb ptrDescProceso : * struct <something>
section .bss
	alignb 4
	global	_ptrDescProceso
_ptrDescProceso:
	resb	4

; glb tamDescProceso : unsigned short
section .bss
	alignb 4
	global	_tamDescProceso
_tamDescProceso:
	resb	2

; glb ptrDescThread : * struct <something>
section .bss
	alignb 4
	global	_ptrDescThread
_ptrDescThread:
	resb	4

; glb tamDescThread : unsigned short
section .bss
	alignb 4
	global	_tamDescThread
_tamDescThread:
	resb	2

; glb ptrDescFichero : * struct <something>
section .bss
	alignb 4
	global	_ptrDescFichero
_ptrDescFichero:
	resb	4

; glb ptrDescRecurso : * struct <something>
section .bss
	alignb 4
	global	_ptrDescRecurso
_ptrDescRecurso:
	resb	4

; glb ptrC2cPFR : * struct <something>
section .bss
	alignb 4
	global	_ptrC2cPFR
_ptrC2cPFR:
	resb	4

; glb ptrE2PFR : * struct <something>
section .bss
	alignb 4
	global	_ptrE2PFR
_ptrE2PFR:
	resb	4

; glb ptrNivelActivacionSO1H : * int
section .bss
	alignb 4
	global	_ptrNivelActivacionSO1H
_ptrNivelActivacionSO1H:
	resb	4

; glb ptrEnHalt : * int
section .bss
	alignb 4
	global	_ptrEnHalt
_ptrEnHalt:
	resb	4

; glb ptrHayTic : * int
section .bss
	alignb 4
	global	_ptrHayTic
_ptrHayTic:
	resb	4

; glb ptrCcbAlEpilogo : * * struct <something>
section .bss
	alignb 4
	global	_ptrCcbAlEpilogo
_ptrCcbAlEpilogo:
	resb	4

; glb ptrSS_Thread : * unsigned short
section .bss
	alignb 4
	global	_ptrSS_Thread
_ptrSS_Thread:
	resb	4

; glb ptrSP_Thread : * unsigned short
section .bss
	alignb 4
	global	_ptrSP_Thread
_ptrSP_Thread:
	resb	4

; glb ptrSS_Kernel : * unsigned short
section .bss
	alignb 4
	global	_ptrSS_Kernel
_ptrSS_Kernel:
	resb	4

; glb ptrSP0_Kernel : * unsigned short
section .bss
	alignb 4
	global	_ptrSP0_Kernel
_ptrSP0_Kernel:
	resb	4

; glb SP0_SO1H : unsigned short
section .bss
	alignb 4
	global	_SP0_SO1H
_SP0_SO1H:
	resb	2

; glb ptrContRodajas : * unsigned
section .bss
	alignb 4
	global	_ptrContRodajas
_ptrContRodajas:
	resb	4

; glb ptrContTicsRodaja : * int
section .bss
	alignb 4
	global	_ptrContTicsRodaja
_ptrContTicsRodaja:
	resb	4

; glb ptrVIOrg : * * (void) void
section .bss
	alignb 4
	global	_ptrVIOrg
_ptrVIOrg:
	resb	4

; glb sigThread : * () int
section .bss
	alignb 4
	global	_sigThread
_sigThread:
	resb	4

; glb activarThread : * (
; prm     tindx : int
;     ) void
section .bss
	alignb 4
	global	_activarThread
_activarThread:
	resb	4

; glb buscarNuevoThreadActual : * (void) void
section .bss
	alignb 4
	global	_buscarNuevoThreadActual
_buscarNuevoThreadActual:
	resb	4

; glb bloquearThreadActual : * (
; prm     rindx : int
;     ) void
section .bss
	alignb 4
	global	_bloquearThreadActual
_bloquearThreadActual:
	resb	4

; glb ptrListaLibres : * * struct <something>
section .bss
	alignb 4
	global	_ptrListaLibres
_ptrListaLibres:
	resb	4

; glb ptrTamBloqueMax : * unsigned short
section .bss
	alignb 4
	global	_ptrTamBloqueMax
_ptrTamBloqueMax:
	resb	4

; glb inicProcesos_x : (void) void
section .text
	global	_inicProcesos_x
_inicProcesos_x:
	push	ebp
	movzx	ebp, sp
	 sub	sp,          8
; loc     i : (@-4): int
; loc     ind : (@-8): int
; loc     <something> : * struct <something>
; loc     <something> : * struct <something>
; RPN'ized expression: "( FALSE , 16 , ptrE2PFR e2DescProceso -> *u &u Libres -> *u &u (something4) , ptrC2cPFR DPLibres + *u &u (something3) inicializarPC2c ) "
; Expanded expression: " 0  16  ptrE2PFR *(4) 0 + 0 +  ptrC2cPFR *(4) 0 +  inicializarPC2c ()16 "
; Fused expression:    "( 0 , 16 , ptrE2PFR + *ax 0 + ax 0 , ptrC2cPFR + *ax 0 , inicializarPC2c )16 "
	push	dword 0
	push	dword 16
section .relod
	dd	L5
section .text
	db	0x66, 0xB8
L5:
	dd	_ptrE2PFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	push	eax
section .relod
	dd	L6
section .text
	db	0x66, 0xB8
L6:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	push	eax
	db	0x9A
section .relot
	dd	L7
section .text
L7:
	dd	_inicializarPC2c
	sub	sp, -16
; loc     <something> : * struct <something>
; loc     <something> : * struct <something>
; RPN'ized expression: "( TRUE , 16 1 + , ptrE2PFR e2DescProceso -> *u &u Ocupados -> *u &u (something9) , ptrC2cPFR DPOcupados + *u &u (something8) inicializarPC2c ) "
; Expanded expression: " 1  17  ptrE2PFR *(4) 0 + 0 +  ptrC2cPFR *(4) 16 +  inicializarPC2c ()16 "
; Fused expression:    "( 1 , 17 , ptrE2PFR + *ax 0 + ax 0 , ptrC2cPFR + *ax 16 , inicializarPC2c )16 "
	push	dword 1
	push	dword 17
section .relod
	dd	L10
section .text
	db	0x66, 0xB8
L10:
	dd	_ptrE2PFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	push	eax
section .relod
	dd	L11
section .text
	db	0x66, 0xB8
L11:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 16
	push	eax
	db	0x9A
section .relot
	dd	L12
section .text
L12:
	dd	_inicializarPC2c
	sub	sp, -16
; loc     <something> : * struct <something>
; loc     <something> : * struct <something>
; RPN'ized expression: "( FALSE , 2010 , ptrE2PFR e2DescThread -> *u &u Libres -> *u &u (something14) , ptrC2cPFR DTLibres + *u &u (something13) inicializarPC2c ) "
; Expanded expression: " 0  2010  ptrE2PFR *(4) 216 + 0 +  ptrC2cPFR *(4) 32 +  inicializarPC2c ()16 "
; Fused expression:    "( 0 , 2010 , ptrE2PFR + *ax 216 + ax 0 , ptrC2cPFR + *ax 32 , inicializarPC2c )16 "
	push	dword 0
	push	dword 2010
section .relod
	dd	L15
section .text
	db	0x66, 0xB8
L15:
	dd	_ptrE2PFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 216
	push	eax
section .relod
	dd	L16
section .text
	db	0x66, 0xB8
L16:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 32
	push	eax
	db	0x9A
section .relot
	dd	L17
section .text
L17:
	dd	_inicializarPC2c
	sub	sp, -16
; loc     <something> : * struct <something>
; loc     <something> : * struct <something>
; RPN'ized expression: "( TRUE , 2010 1 + , ptrE2PFR e2DescThread -> *u &u Ocupados -> *u &u (something19) , ptrC2cPFR DTOcupados + *u &u (something18) inicializarPC2c ) "
; Expanded expression: " 1  2011  ptrE2PFR *(4) 216 + 0 +  ptrC2cPFR *(4) 48 +  inicializarPC2c ()16 "
; Fused expression:    "( 1 , 2011 , ptrE2PFR + *ax 216 + ax 0 , ptrC2cPFR + *ax 48 , inicializarPC2c )16 "
	push	dword 1
	push	dword 2011
section .relod
	dd	L20
section .text
	db	0x66, 0xB8
L20:
	dd	_ptrE2PFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 216
	push	eax
section .relod
	dd	L21
section .text
	db	0x66, 0xB8
L21:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 48
	push	eax
	db	0x9A
section .relot
	dd	L22
section .text
L22:
	dd	_inicializarPC2c
	sub	sp, -16
; loc     <something> : * struct <something>
; loc     <something> : * struct <something>
; RPN'ized expression: "( FALSE , 2010 , ptrE2PFR e2Preparados -> *u &u (something24) , ptrC2cPFR TPreparados + *u &u (something23) inicializarPC2c ) "
; Expanded expression: " 0  2010  ptrE2PFR *(4) 49512 +  ptrC2cPFR *(4) 64 +  inicializarPC2c ()16 "
; Fused expression:    "( 0 , 2010 , ptrE2PFR + *ax 49512 , ptrC2cPFR + *ax 64 , inicializarPC2c )16 "
	push	dword 0
	push	dword 2010
section .relod
	dd	L25
section .text
	db	0x66, 0xB8
L25:
	dd	_ptrE2PFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 49512
	push	eax
section .relod
	dd	L26
section .text
	db	0x66, 0xB8
L26:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 64
	push	eax
	db	0x9A
section .relot
	dd	L27
section .text
L27:
	dd	_inicializarPC2c
	sub	sp, -16
; loc     <something> : * struct <something>
; loc     <something> : * struct <something>
; RPN'ized expression: "( FALSE , 2010 , ptrE2PFR e2Urgentes -> *u &u (something29) , ptrC2cPFR TUrgentes + *u &u (something28) inicializarPC2c ) "
; Expanded expression: " 0  2010  ptrE2PFR *(4) 73644 +  ptrC2cPFR *(4) 80 +  inicializarPC2c ()16 "
; Fused expression:    "( 0 , 2010 , ptrE2PFR + *ax 73644 , ptrC2cPFR + *ax 80 , inicializarPC2c )16 "
	push	dword 0
	push	dword 2010
section .relod
	dd	L30
section .text
	db	0x66, 0xB8
L30:
	dd	_ptrE2PFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 73644
	push	eax
section .relod
	dd	L31
section .text
	db	0x66, 0xB8
L31:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 80
	push	eax
	db	0x9A
section .relot
	dd	L32
section .text
L32:
	dd	_inicializarPC2c
	sub	sp, -16
; loc     <something> : * struct <something>
; loc     <something> : * struct <something>
; RPN'ized expression: "( FALSE , 16 , ptrE2PFR e2POrdenados -> *u &u (something34) , ptrC2cPFR POrdenados + *u &u (something33) inicializarPC2c ) "
; Expanded expression: " 0  16  ptrE2PFR *(4) 97776 +  ptrC2cPFR *(4) 96 +  inicializarPC2c ()16 "
; Fused expression:    "( 0 , 16 , ptrE2PFR + *ax 97776 , ptrC2cPFR + *ax 96 , inicializarPC2c )16 "
	push	dword 0
	push	dword 16
section .relod
	dd	L35
section .text
	db	0x66, 0xB8
L35:
	dd	_ptrE2PFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 97776
	push	eax
section .relod
	dd	L36
section .text
	db	0x66, 0xB8
L36:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 96
	push	eax
	db	0x9A
section .relot
	dd	L37
section .text
L37:
	dd	_inicializarPC2c
	sub	sp, -16
; loc     <something> : * struct <something>
; loc     <something> : * struct <something>
; RPN'ized expression: "( FALSE , 2010 , ptrE2PFR e2TDormidos -> *u &u (something39) , ptrC2cPFR TDormidos + *u &u (something38) inicializarPC2c ) "
; Expanded expression: " 0  2010  ptrE2PFR *(4) 97980 +  ptrC2cPFR *(4) 112 +  inicializarPC2c ()16 "
; Fused expression:    "( 0 , 2010 , ptrE2PFR + *ax 97980 , ptrC2cPFR + *ax 112 , inicializarPC2c )16 "
	push	dword 0
	push	dword 2010
section .relod
	dd	L40
section .text
	db	0x66, 0xB8
L40:
	dd	_ptrE2PFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 97980
	push	eax
section .relod
	dd	L41
section .text
	db	0x66, 0xB8
L41:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 112
	push	eax
	db	0x9A
section .relot
	dd	L42
section .text
L42:
	dd	_inicializarPC2c
	sub	sp, -16
; for
; RPN'ized expression: "i 1 = "
; Expanded expression: "(@-4) 1 =(4) "
; Fused expression:    "=(204) *(@-4) 1 "
	mov	eax, 1
	mov	[bp-4], eax
L43:
; RPN'ized expression: "i numER 1 - < "
; Expanded expression: "(@-4) *(4) numER *(4) 1 - <u "
; Fused expression:    "numER - *ax 1 <u *(@-4) ax IF! "
section .relod
	dd	L47
section .text
	db	0x66, 0xB8
L47:
	dd	_numER
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	dec	eax
	mov	ecx, eax
	mov	eax, [bp-4]
	cmp	eax, ecx
	jae	L46
; RPN'ized expression: "i ++p "
; Expanded expression: "(@-4) ++p(4) "
; {
; RPN'ized expression: "ind i = "
; Expanded expression: "(@-8) (@-4) *(4) =(4) "
; Fused expression:    "=(204) *(@-8) *(@-4) "
	mov	eax, [bp-4]
	mov	[bp-8], eax
; RPN'ized expression: "ptrDescProceso i + *u &u pid -> *u i = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 0 + (@-4) *(4) =(4) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 0 =(204) *ax *(@-4) "
section .relod
	dd	L48
section .text
	db	0x66, 0xB8
L48:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	mov	ebx, eax
	mov	eax, [bp-4]
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescProceso i + *u &u noStatus -> *u TRUE = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 4 + 1 =(4) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 4 =(204) *ax 1 "
section .relod
	dd	L49
section .text
	db	0x66, 0xB8
L49:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 4
	mov	ebx, eax
	mov	eax, 1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescProceso i + *u &u ppindx -> *u 1 -u = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 12 + -1 =(4) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 12 =(204) *ax -1 "
section .relod
	dd	L50
section .text
	db	0x66, 0xB8
L50:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescProceso i + *u &u hpindx -> *u 1 -u = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 16 + -1 =(4) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 16 =(204) *ax -1 "
section .relod
	dd	L51
section .text
	db	0x66, 0xB8
L51:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 16
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; loc         <something> : * struct <something>
; loc         <something> : * struct <something>
; RPN'ized expression: "( FALSE , 16 i + , ptrE2PFR e2Hijos -> *u &u (something53) , ptrDescProceso i + *u &u c2cHijos -> *u &u (something52) inicializarPC2c ) "
; Expanded expression: " 0  16 (@-4) *(4) +  ptrE2PFR *(4) 24816 +  ptrDescProceso *(4) (@-4) *(4) 292 * + 20 +  inicializarPC2c ()16 "
; Fused expression:    "( 0 , + 16 *(@-4) , ptrE2PFR + *ax 24816 , ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 20 , inicializarPC2c )16 "
	push	dword 0
	mov	eax, 16
	add	eax, [bp-4]
	push	eax
section .relod
	dd	L54
section .text
	db	0x66, 0xB8
L54:
	dd	_ptrE2PFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 24816
	push	eax
section .relod
	dd	L55
section .text
	db	0x66, 0xB8
L55:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 20
	push	eax
	db	0x9A
section .relot
	dd	L56
section .text
L56:
	dd	_inicializarPC2c
	sub	sp, -16
; loc         <something> : * struct <something>
; loc         <something> : * struct <something>
; RPN'ized expression: "( FALSE , 2010 i + , ptrE2PFR e2Threads -> *u &u (something58) , ptrDescProceso i + *u &u c2cThreads -> *u &u (something57) inicializarPC2c ) "
; Expanded expression: " 0  2010 (@-4) *(4) +  ptrE2PFR *(4) 25200 +  ptrDescProceso *(4) (@-4) *(4) 292 * + 36 +  inicializarPC2c ()16 "
; Fused expression:    "( 0 , + 2010 *(@-4) , ptrE2PFR + *ax 25200 , ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 36 , inicializarPC2c )16 "
	push	dword 0
	mov	eax, 2010
	add	eax, [bp-4]
	push	eax
section .relod
	dd	L59
section .text
	db	0x66, 0xB8
L59:
	dd	_ptrE2PFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 25200
	push	eax
section .relod
	dd	L60
section .text
	db	0x66, 0xB8
L60:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 36
	push	eax
	db	0x9A
section .relot
	dd	L61
section .text
L61:
	dd	_inicializarPC2c
	sub	sp, -16
; loc         <something> : struct <something>
; RPN'ized expression: "ptrDescProceso i + *u &u CSProc -> *u reubicacion ind + *u &u destino -> *u <something62> sizeof - = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 52 + reubicacion *(4) (@-8) *(4) 88 * + 84 + *(4) 32u - =(2) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 52 push-ax reubicacion push-ax * *(@-8) 88 + **sp ax + ax 84 - *ax 32u =(172) **sp ax "
section .relod
	dd	L63
section .text
	db	0x66, 0xB8
L63:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 52
	push	eax
section .relod
	dd	L64
section .text
	db	0x66, 0xB8
L64:
	dd	_reubicacion
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 88
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 84
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	sub	eax, 32
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescProceso i + *u &u tam -> *u reubicacion ind + *u &u entradaDF -> *u &u SS -> *u = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 60 + reubicacion *(4) (@-8) *(4) 88 * + 0 + 72 + *(2) =(2) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 60 push-ax reubicacion push-ax * *(@-8) 88 + **sp ax + ax 0 + ax 72 =(170) **sp *ax "
section .relod
	dd	L65
section .text
	db	0x66, 0xB8
L65:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 60
	push	eax
section .relod
	dd	L66
section .text
	db	0x66, 0xB8
L66:
	dd	_reubicacion
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 88
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 72
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescProceso i + *u &u tamFichero -> *u reubicacion ind + *u &u entradaDF -> *u &u tam -> *u = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 64 + reubicacion *(4) (@-8) *(4) 88 * + 0 + 60 + *(4) =(4) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 64 push-ax reubicacion push-ax * *(@-8) 88 + **sp ax + ax 0 + ax 60 =(204) **sp *ax "
section .relod
	dd	L67
section .text
	db	0x66, 0xB8
L67:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 64
	push	eax
section .relod
	dd	L68
section .text
	db	0x66, 0xB8
L68:
	dd	_reubicacion
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 88
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 60
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "( ptrDescProceso i + *u &u programa -> *u &u , reubicacion ind + *u &u entradaDF -> *u &u nombre -> *u &u strcpy ) "
; Expanded expression: " ptrDescProceso *(4) (@-4) *(4) 292 * + 68 +  reubicacion *(4) (@-8) *(4) 88 * + 0 + 0 +  strcpy ()8 "
; Fused expression:    "( ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 68 , reubicacion push-ax * *(@-8) 88 + **sp ax + ax 0 + ax 0 , strcpy )8 "
section .relod
	dd	L69
section .text
	db	0x66, 0xB8
L69:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 68
	push	eax
section .relod
	dd	L70
section .text
	db	0x66, 0xB8
L70:
	dd	_reubicacion
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 88
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	push	eax
	db	0x9A
section .relot
	dd	L71
section .text
L71:
	dd	_strcpy
	sub	sp, -8
; loc         <something> : * struct <something>
; RPN'ized expression: "( ptrC2cPFR DPOcupados + *u &u (something72) , i apilarPC2c ) "
; Expanded expression: " ptrC2cPFR *(4) 16 +  (@-4) *(4)  apilarPC2c ()8 "
; Fused expression:    "( ptrC2cPFR + *ax 16 , *(4) (@-4) , apilarPC2c )8 "
section .relod
	dd	L73
section .text
	db	0x66, 0xB8
L73:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 16
	push	eax
	push	dword [bp-4]
	db	0x9A
section .relot
	dd	L74
section .text
L74:
	dd	_apilarPC2c
	sub	sp, -8
; loc         <something> : * struct <something>
; RPN'ized expression: "( ptrC2cPFR POrdenados + *u &u (something75) , i apilarPC2c ) "
; Expanded expression: " ptrC2cPFR *(4) 96 +  (@-4) *(4)  apilarPC2c ()8 "
; Fused expression:    "( ptrC2cPFR + *ax 96 , *(4) (@-4) , apilarPC2c )8 "
section .relod
	dd	L76
section .text
	db	0x66, 0xB8
L76:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 96
	push	eax
	push	dword [bp-4]
	db	0x9A
section .relot
	dd	L77
section .text
L77:
	dd	_apilarPC2c
	sub	sp, -8
; RPN'ized expression: "ptrDescThread i + *u &u SP0 -> *u reubicacion ind + *u &u entradaDF -> *u &u SP0 -> *u = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 38 + reubicacion *(4) (@-8) *(4) 88 * + 0 + 74 + *(2) =(2) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 38 push-ax reubicacion push-ax * *(@-8) 88 + **sp ax + ax 0 + ax 74 =(170) **sp *ax "
section .relod
	dd	L78
section .text
	db	0x66, 0xB8
L78:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 38
	push	eax
section .relod
	dd	L79
section .text
	db	0x66, 0xB8
L79:
	dd	_reubicacion
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 88
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 74
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescThread i + *u &u tid -> *u i = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 0 + (@-4) *(4) =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 0 =(204) *ax *(@-4) "
section .relod
	dd	L80
section .text
	db	0x66, 0xB8
L80:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	mov	ebx, eax
	mov	eax, [bp-4]
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u estado -> *u preparado = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 4 + 1 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 4 =(204) *ax 1 "
section .relod
	dd	L81
section .text
	db	0x66, 0xB8
L81:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 4
	mov	ebx, eax
	mov	eax, 1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u tw -> *u &u DS -> *u ptrDescProceso i + *u &u CSProc -> *u 4 >> = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 0 + ptrDescProceso *(4) (@-4) *(4) 292 * + 52 + *(2) 4 >> =(2) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 0 push-ax ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 52 >> *ax 4 =(172) **sp ax "
section .relod
	dd	L82
section .text
	db	0x66, 0xB8
L82:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	push	eax
section .relod
	dd	L83
section .text
	db	0x66, 0xB8
L83:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 52
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	sar	eax, 4
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u tw -> *u &u ES -> *u ptrDescProceso i + *u &u CSProc -> *u 4 >> = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 2 + ptrDescProceso *(4) (@-4) *(4) 292 * + 52 + *(2) 4 >> =(2) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 2 push-ax ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 52 >> *ax 4 =(172) **sp ax "
section .relod
	dd	L84
section .text
	db	0x66, 0xB8
L84:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 2
	push	eax
section .relod
	dd	L85
section .text
	db	0x66, 0xB8
L85:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 52
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	sar	eax, 4
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u td -> *u &u EDI -> *u 0 = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 4 + 0 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 4 =(204) *ax 0 "
section .relod
	dd	L86
section .text
	db	0x66, 0xB8
L86:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 4
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u td -> *u &u ESI -> *u 0 = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 8 + 0 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 8 =(204) *ax 0 "
section .relod
	dd	L87
section .text
	db	0x66, 0xB8
L87:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 8
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u td -> *u &u EBP -> *u ptrDescThread i + *u &u SP0 -> *u = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 12 + ptrDescThread *(4) (@-4) *(4) 40 * + 38 + *(2) =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 12 push-ax ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 38 =(202) **sp *ax "
section .relod
	dd	L88
section .text
	db	0x66, 0xB8
L88:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 12
	push	eax
section .relod
	dd	L89
section .text
	db	0x66, 0xB8
L89:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 38
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u td -> *u &u ESP -> *u ptrDescThread i + *u &u SP0 -> *u 6 - = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 16 + ptrDescThread *(4) (@-4) *(4) 40 * + 38 + *(2) 6 - =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 16 push-ax ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 38 - *ax 6 =(204) **sp ax "
section .relod
	dd	L90
section .text
	db	0x66, 0xB8
L90:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 16
	push	eax
section .relod
	dd	L91
section .text
	db	0x66, 0xB8
L91:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 38
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	sub	eax, 6
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u td -> *u &u EBX -> *u 0 = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 20 + 0 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 20 =(204) *ax 0 "
section .relod
	dd	L92
section .text
	db	0x66, 0xB8
L92:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 20
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u td -> *u &u EDX -> *u 0 = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 24 + 0 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 24 =(204) *ax 0 "
section .relod
	dd	L93
section .text
	db	0x66, 0xB8
L93:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 24
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u td -> *u &u ECX -> *u 0 = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 28 + 0 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 28 =(204) *ax 0 "
section .relod
	dd	L94
section .text
	db	0x66, 0xB8
L94:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 28
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u td -> *u &u EAX -> *u 0 = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 32 + 0 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 32 =(204) *ax 0 "
section .relod
	dd	L95
section .text
	db	0x66, 0xB8
L95:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 32
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u tw -> *u &u IP -> *u reubicacion ind + *u &u entradaDF -> *u &u start -> *u = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 36 + reubicacion *(4) (@-8) *(4) 88 * + 0 + 64 + *(4) =(2) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 36 push-ax reubicacion push-ax * *(@-8) 88 + **sp ax + ax 0 + ax 64 =(172) **sp *ax "
section .relod
	dd	L96
section .text
	db	0x66, 0xB8
L96:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 36
	push	eax
section .relod
	dd	L97
section .text
	db	0x66, 0xB8
L97:
	dd	_reubicacion
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 88
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 64
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u tw -> *u &u CS -> *u ptrDescProceso i + *u &u CSProc -> *u 4 >> = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 38 + ptrDescProceso *(4) (@-4) *(4) 292 * + 52 + *(2) 4 >> =(2) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 38 push-ax ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 52 >> *ax 4 =(172) **sp ax "
section .relod
	dd	L98
section .text
	db	0x66, 0xB8
L98:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 38
	push	eax
section .relod
	dd	L99
section .text
	db	0x66, 0xB8
L99:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 52
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	sar	eax, 4
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescThread i + *u &u trama -> *u tw -> *u &u Flags -> *u 514 = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 12 + *(4) 0 + 40 + 514 =(2) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 12 + *ax 0 + ax 40 =(172) *ax 514 "
section .relod
	dd	L100
section .text
	db	0x66, 0xB8
L100:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 40
	mov	ebx, eax
	mov	eax, 514
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescThread i + *u &u noStatus -> *u TRUE = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 16 + 1 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 16 =(204) *ax 1 "
section .relod
	dd	L101
section .text
	db	0x66, 0xB8
L101:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 16
	mov	ebx, eax
	mov	eax, 1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u ptindx -> *u 1 -u = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 24 + -1 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 24 =(204) *ax -1 "
section .relod
	dd	L102
section .text
	db	0x66, 0xB8
L102:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 24
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u htindx -> *u 1 -u = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 28 + -1 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 28 =(204) *ax -1 "
section .relod
	dd	L103
section .text
	db	0x66, 0xB8
L103:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 28
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u pindx -> *u i = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 32 + (@-4) *(4) =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 32 =(204) *ax *(@-4) "
section .relod
	dd	L104
section .text
	db	0x66, 0xB8
L104:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 32
	mov	ebx, eax
	mov	eax, [bp-4]
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; loc         <something> : * struct <something>
; RPN'ized expression: "( ptrC2cPFR DTOcupados + *u &u (something105) , i apilarPC2c ) "
; Expanded expression: " ptrC2cPFR *(4) 48 +  (@-4) *(4)  apilarPC2c ()8 "
; Fused expression:    "( ptrC2cPFR + *ax 48 , *(4) (@-4) , apilarPC2c )8 "
section .relod
	dd	L106
section .text
	db	0x66, 0xB8
L106:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 48
	push	eax
	push	dword [bp-4]
	db	0x9A
section .relot
	dd	L107
section .text
L107:
	dd	_apilarPC2c
	sub	sp, -8
; loc         <something> : * struct <something>
; RPN'ized expression: "( ptrDescProceso i + *u &u c2cThreads -> *u &u (something108) , i apilarPC2c ) "
; Expanded expression: " ptrDescProceso *(4) (@-4) *(4) 292 * + 36 +  (@-4) *(4)  apilarPC2c ()8 "
; Fused expression:    "( ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 36 , *(4) (@-4) , apilarPC2c )8 "
section .relod
	dd	L109
section .text
	db	0x66, 0xB8
L109:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 36
	push	eax
	push	dword [bp-4]
	db	0x9A
section .relot
	dd	L110
section .text
L110:
	dd	_apilarPC2c
	sub	sp, -8
; }
L44:
; Fused expression:    "++p(4) *(@-4) "
	mov	eax, [bp-4]
	inc	dword [bp-4]
	jmp	L43
L46:
; loc     <something> : struct <something>
; RPN'ized expression: "ptrSS_Kernel *u dirInicial <something111> sizeof - 4 >> reubicacion 2 + *u &u entradaDF -> *u &u SS -> *u + = "
; Expanded expression: "ptrSS_Kernel *(4) dirInicial *(4) 32u - 4 >>u reubicacion *(4) 176 + 0 + 72 + *(2) + =(2) "
; Fused expression:    "ptrSS_Kernel *(4) ax push-ax dirInicial - *ax 32u >>u ax 4 push-ax reubicacion + *ax 176 + ax 0 + ax 72 + *sp *ax =(172) **sp ax "
section .relod
	dd	L112
section .text
	db	0x66, 0xB8
L112:
	dd	_ptrSS_Kernel
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	push	eax
section .relod
	dd	L113
section .text
	db	0x66, 0xB8
L113:
	dd	_dirInicial
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	sub	eax, 32
	shr	eax, 4
	push	eax
section .relod
	dd	L114
section .text
	db	0x66, 0xB8
L114:
	dd	_reubicacion
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 176
	add	eax, 72
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	movzx	ecx, word [si]
	pop	eax
	add	eax, ecx
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrSP0_Kernel *u reubicacion 2 + *u &u entradaDF -> *u &u SP0 -> *u = "
; Expanded expression: "ptrSP0_Kernel *(4) reubicacion *(4) 176 + 0 + 74 + *(2) =(2) "
; Fused expression:    "ptrSP0_Kernel *(4) ax push-ax reubicacion + *ax 176 + ax 0 + ax 74 =(170) **sp *ax "
section .relod
	dd	L115
section .text
	db	0x66, 0xB8
L115:
	dd	_ptrSP0_Kernel
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	push	eax
section .relod
	dd	L116
section .text
	db	0x66, 0xB8
L116:
	dd	_reubicacion
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 176
	add	eax, 74
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; for
; RPN'ized expression: "i 16 1 - = "
; Expanded expression: "(@-4) 15 =(4) "
; Fused expression:    "=(204) *(@-4) 15 "
	mov	eax, 15
	mov	[bp-4], eax
L117:
; RPN'ized expression: "i numER 1 - >= "
; Expanded expression: "(@-4) *(4) numER *(4) 1 - >=u "
; Fused expression:    "numER - *ax 1 >=u *(@-4) ax IF! "
section .relod
	dd	L121
section .text
	db	0x66, 0xB8
L121:
	dd	_numER
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	dec	eax
	mov	ecx, eax
	mov	eax, [bp-4]
	cmp	eax, ecx
	jb	L120
; RPN'ized expression: "i --p "
; Expanded expression: "(@-4) --p(4) "
; {
; RPN'ized expression: "ptrDescProceso i + *u &u pid -> *u 1 -u = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 0 + -1 =(4) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 0 =(204) *ax -1 "
section .relod
	dd	L122
section .text
	db	0x66, 0xB8
L122:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescProceso i + *u &u uid -> *u 1 -u = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 284 + -1 =(4) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 284 =(204) *ax -1 "
section .relod
	dd	L123
section .text
	db	0x66, 0xB8
L123:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 284
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescProceso i + *u &u gid -> *u 1 -u = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 288 + -1 =(4) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 288 =(204) *ax -1 "
section .relod
	dd	L124
section .text
	db	0x66, 0xB8
L124:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 288
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescProceso i + *u &u ppindx -> *u 1 -u = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 12 + -1 =(4) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 12 =(204) *ax -1 "
section .relod
	dd	L125
section .text
	db	0x66, 0xB8
L125:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescProceso i + *u &u hpindx -> *u 1 -u = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 16 + -1 =(4) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 16 =(204) *ax -1 "
section .relod
	dd	L126
section .text
	db	0x66, 0xB8
L126:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 16
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescProceso i + *u &u CSProc -> *u 0 = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 52 + 0 =(2) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 52 =(172) *ax 0 "
section .relod
	dd	L127
section .text
	db	0x66, 0xB8
L127:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 52
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescProceso i + *u &u tam -> *u 0 = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 60 + 0 =(2) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 60 =(172) *ax 0 "
section .relod
	dd	L128
section .text
	db	0x66, 0xB8
L128:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 60
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescProceso i + *u &u tamCodigo -> *u 0 = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 54 + 0 =(2) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 54 =(172) *ax 0 "
section .relod
	dd	L129
section .text
	db	0x66, 0xB8
L129:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 54
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescProceso i + *u &u desplBSS -> *u 0 = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 56 + 0 =(2) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 56 =(172) *ax 0 "
section .relod
	dd	L130
section .text
	db	0x66, 0xB8
L130:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 56
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescProceso i + *u &u desplPila -> *u 0 = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 58 + 0 =(2) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 58 =(172) *ax 0 "
section .relod
	dd	L131
section .text
	db	0x66, 0xB8
L131:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 58
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescProceso i + *u &u tamFichero -> *u 0 = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 64 + 0 =(4) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 64 =(204) *ax 0 "
section .relod
	dd	L132
section .text
	db	0x66, 0xB8
L132:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 64
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; loc         <something> : char
; RPN'ized expression: "ptrDescProceso i + *u &u programa -> *u 0 + *u 0 (something133) = "
; Expanded expression: "ptrDescProceso *(4) (@-4) *(4) 292 * + 68 + 0 =(-1) "
; Fused expression:    "ptrDescProceso push-ax * *(@-4) 292 + **sp ax + ax 68 =(124) *ax 0 "
section .relod
	dd	L134
section .text
	db	0x66, 0xB8
L134:
	dd	_ptrDescProceso
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 68
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], al
	movsx	eax, al
; loc         <something> : * struct <something>
; RPN'ized expression: "( ptrC2cPFR DPLibres + *u &u (something135) , i apilarPC2c ) "
; Expanded expression: " ptrC2cPFR *(4) 0 +  (@-4) *(4)  apilarPC2c ()8 "
; Fused expression:    "( ptrC2cPFR + *ax 0 , *(4) (@-4) , apilarPC2c )8 "
section .relod
	dd	L136
section .text
	db	0x66, 0xB8
L136:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	push	eax
	push	dword [bp-4]
	db	0x9A
section .relot
	dd	L137
section .text
L137:
	dd	_apilarPC2c
	sub	sp, -8
; }
L118:
; Fused expression:    "--p(4) *(@-4) "
	mov	eax, [bp-4]
	dec	dword [bp-4]
	jmp	L117
L120:
; for
; RPN'ized expression: "i 2010 1 - = "
; Expanded expression: "(@-4) 2009 =(4) "
; Fused expression:    "=(204) *(@-4) 2009 "
	mov	eax, 2009
	mov	[bp-4], eax
L138:
; RPN'ized expression: "i 0 > "
; Expanded expression: "(@-4) *(4) 0 > "
; Fused expression:    "> *(@-4) 0 IF! "
	mov	eax, [bp-4]
	cmp	eax, 0
	jle	L141
; RPN'ized expression: "i --p "
; Expanded expression: "(@-4) --p(4) "
; {
; RPN'ized expression: "ptrDescThread i + *u &u tid -> *u 1 -u = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 0 + -1 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 0 =(204) *ax -1 "
section .relod
	dd	L142
section .text
	db	0x66, 0xB8
L142:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u estado -> *u libre = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 4 + 0 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 4 =(204) *ax 0 "
section .relod
	dd	L143
section .text
	db	0x66, 0xB8
L143:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 4
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u pindx -> *u 1 -u = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 32 + -1 =(4) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 32 =(204) *ax -1 "
section .relod
	dd	L144
section .text
	db	0x66, 0xB8
L144:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 32
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "ptrDescThread i + *u &u SSThread -> *u 0 = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 36 + 0 =(2) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 36 =(172) *ax 0 "
section .relod
	dd	L145
section .text
	db	0x66, 0xB8
L145:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 36
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "ptrDescThread i + *u &u SP0 -> *u 0 = "
; Expanded expression: "ptrDescThread *(4) (@-4) *(4) 40 * + 38 + 0 =(2) "
; Fused expression:    "ptrDescThread push-ax * *(@-4) 40 + **sp ax + ax 38 =(172) *ax 0 "
section .relod
	dd	L146
section .text
	db	0x66, 0xB8
L146:
	dd	_ptrDescThread
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	add	eax, 38
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; loc         <something> : * struct <something>
; RPN'ized expression: "( ptrC2cPFR DTLibres + *u &u (something147) , i apilarPC2c ) "
; Expanded expression: " ptrC2cPFR *(4) 32 +  (@-4) *(4)  apilarPC2c ()8 "
; Fused expression:    "( ptrC2cPFR + *ax 32 , *(4) (@-4) , apilarPC2c )8 "
section .relod
	dd	L148
section .text
	db	0x66, 0xB8
L148:
	dd	_ptrC2cPFR
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 32
	push	eax
	push	dword [bp-4]
	db	0x9A
section .relot
	dd	L149
section .text
L149:
	dd	_apilarPC2c
	sub	sp, -8
; }
L139:
; Fused expression:    "--p(4) *(@-4) "
	mov	eax, [bp-4]
	dec	dword [bp-4]
	jmp	L138
L141:
L1:
	db	0x66
	leave
	retf
L150:

section .fxnsz noalloc
	dd	L150 - _inicProcesos_x


	extern	_inicializarPC2c
	extern	_numER
	extern	_reubicacion
	extern	_strcpy
	extern	_apilarPC2c
	extern	_dirInicial

; Syntax/declaration table/stack:
; Bytes used: 10985/40960


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
; Ident nombreDF_t
; Ident comandoDF_t
; Ident so1h_0_DF
; Ident datos_DF
; Ident so1h_k_DF
; Ident proceso_DF
; Ident dram_DF
; Ident tipoDF_t
; Ident start
; Ident otro
; Ident SS
; Ident entradaDF_t
; Ident dirInicial
; Ident dirCargaFichero
; Ident dirFinal
; Ident entradaDF
; Ident origen
; Ident destino
; Ident reubicacion_t
; Ident reubicacion
; Ident numER
; Ident inicProcesos_x
; Ident size_t
; Ident memset
; Ident memcpy
; Ident memmove
; Ident memchr
; Ident memcmp
; Ident strcpy
; Ident strncpy
; Ident strxfrm
; Ident strcat
; Ident strncat
; Ident strlen
; Ident strchr
; Ident strrchr
; Ident strstr
; Ident strspn
; Ident strcspn
; Ident strpbrk
; Ident strtok
; Ident strcmp
; Ident strncmp
; Ident strcoll
; Ident strerror
; Bytes used: 3810/16384

; Next label number: 151
; Compilation succeeded.
