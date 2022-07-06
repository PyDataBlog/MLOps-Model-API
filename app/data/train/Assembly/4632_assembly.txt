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
; glb ptrTVI : * * (void) void
; glb valorIMR : (void) unsigned short
; glb establecerIMR : (
; prm     nuevoIMR : unsigned short
;     ) void
; glb mask_pic1 : (
; prm     irq : unsigned char
;     ) void
; glb mask_pic2 : (
; prm     irq : unsigned char
;     ) void
; glb unmask_pic1 : (
; prm     irq : unsigned char
;     ) void
; glb unmask_pic2 : (
; prm     irq : unsigned char
;     ) void
; glb get_pic1_isr : (void) unsigned char
; glb get_pic2_isr : (void) unsigned char
; glb set_pics : (
; prm     irq0 : unsigned char
; prm     irq8 : unsigned char
;     ) void
; glb pic_setup : (void) void
; glb enable_hwirq : (
; prm     hwirq : int
; prm     rti : * (void) void
;     ) void
; glb startBin : (void) void
; glb modoSO1 : (void) int
; glb unidadBIOS : (void) unsigned char
; glb CS_SO1H : unsigned short
; glb RO_SO1H : unsigned short
; glb DS_SO1H : unsigned short
; glb BSS_SO1H : unsigned short
; glb SS_SO1H : unsigned short
; glb SS_Kernel : unsigned short
; glb IMRInicial : unsigned short
; glb obtenerMapa : (void) void
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
; RPN'ized expression: "12 "
; Expanded expression: "12 "
; Expression value: 12
; RPN'ized expression: "80 "
; Expanded expression: "80 "
; Expression value: 80
; RPN'ized expression: "10 "
; Expanded expression: "10 "
; Expression value: 10
; glb descProcesoExt_t : struct <something>
; glb descThreadExt_t : struct <something>
; RPN'ized expression: "16 "
; Expanded expression: "16 "
; Expression value: 16
; glb descProceso : [16u] struct <something>
; RPN'ized expression: "2010 "
; Expanded expression: "2010 "
; Expression value: 2010
; glb descThread : [2010u] struct <something>
; RPN'ized expression: "20 "
; Expanded expression: "20 "
; Expression value: 20
; glb descFichero : [20u] struct <something>
; RPN'ized expression: "14 "
; Expanded expression: "14 "
; Expression value: 14
; glb descRecurso : [14u] struct <something>
; RPN'ized expression: "numColasPFR "
; Expanded expression: "12 "
; Expression value: 12
; glb c2cPFR : [12u] struct <something>
; glb e2PFR : struct <something>
; glb descCcbAlEpilogo : struct <something>
; glb ccbAlEpilogo : * struct <something>
; glb tramaThread : * union <something>
; glb tramaTarea : * union <something>
; glb indThreadActual : int
; glb indProcesoActual : int
; glb indThreadDeSuperficie : int
; glb contRodajas : unsigned
; glb contTicsRodaja : int
; glb contadorTimer00 : unsigned short
; glb contOcioso : int
; glb nuevoPid : (void) int
; glb nuevoTid : (void) int
; glb indice : (
; prm     tid : int
;     ) int
; glb sigThread : (void) int
; glb activarThread : (
; prm     tindx : int
;     ) int
; glb registrarEnPOrdenados : (
; prm     pindx : int
;     ) void
; glb crearThread : (
; prm     funcion : * (
; prm         <something> : * void
;         ) * void
; prm     SP0 : unsigned short
; prm     arg : * void
; prm     pindx : int
;     ) int
; glb crearProceso : (
; prm     segmento : unsigned short
; prm     tam : unsigned short
; prm     tamFich : unsigned
; prm     programa : * char
; prm     comando : * char
; prm     pindx : int
;     ) int
; glb inicProcesos : (void) void
; glb resetPids : (void) void
; glb resetTids : (void) void
; glb terminarThreadIndx : (
; prm     tindx : int
;     ) int
; glb eliminarThreadIndx : (
; prm     tindx : int
;     ) int
; glb terminarProcIndx : (
; prm     pindx : int
;     ) int
; glb eliminarProcIndx : (
; prm     pindx : int
;     ) int
; glb matarThreadIndx : (
; prm     tindx : int
;     ) int
; glb matarProcIndx : (
; prm     pindx : int
;     ) int
; glb link_procs : (void) void
; glb SS_Thread : unsigned short
; glb SP_Thread : unsigned short
; glb SS_Tarea : unsigned short
; glb SP_Tarea : unsigned short
; glb nivelActivacionSO1H : int
; glb nVIntActual : int
; glb enHalt : int
; glb activarAlEpilogo1 : int
; glb hayTic : int
; glb setKernelStack : (void) void
; glb setThreadStack : (
; prm     SS_Thread : unsigned short
; prm     SP_Thread : unsigned short
;     ) void
; glb reg_DL : (void) unsigned
; glb prepararDesbloqueadosUrgentes : (void) void
; glb buscarNuevoThreadActual : (void) void
; glb bloquearThreadActual : (
; prm     rindx : int
;     ) void
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
; glb isr_SO1H : (void) void
; glb link_llamadas : (void) void
%define NUM_MATRICULA 0x0123
 section .text           
   global _link_llamadas 
 _link_llamadas:         

; glb so1_manejador_00 : (void) void
; glb so1_manejador_01 : (void) void
; glb so1_manejador_02 : (void) void
; glb so1_manejador_03 : (void) void
; glb so1_manejador_0b : (void) void
; glb so1_manejador_Nulo : (void) void
section .text
	global	_so1_manejador_Nulo
_so1_manejador_Nulo:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
L1:
	db	0x66
	leave
	retf
L3:

section .fxnsz noalloc
	dd	L3 - _so1_manejador_Nulo

; glb manejador : [0u] * (void) void
section .data
	align 4
	global	_manejador
_manejador:
; =
; RPN'ized expression: "so1_manejador_00 "
; Expanded expression: "so1_manejador_00 "
section .relod
	dd	L4
section .data
L4:
	dd	_so1_manejador_00
; RPN'ized expression: "so1_manejador_01 "
; Expanded expression: "so1_manejador_01 "
section .relod
	dd	L5
section .data
L5:
	dd	_so1_manejador_01
; RPN'ized expression: "so1_manejador_02 "
; Expanded expression: "so1_manejador_02 "
section .relod
	dd	L6
section .data
L6:
	dd	_so1_manejador_02
; RPN'ized expression: "so1_manejador_03 "
; Expanded expression: "so1_manejador_03 "
section .relod
	dd	L7
section .data
L7:
	dd	_so1_manejador_03
; RPN'ized expression: "so1_manejador_Nulo "
; Expanded expression: "so1_manejador_Nulo "
section .relod
	dd	L8
section .data
L8:
	dd	_so1_manejador_Nulo
; RPN'ized expression: "so1_manejador_Nulo "
; Expanded expression: "so1_manejador_Nulo "
section .relod
	dd	L9
section .data
L9:
	dd	_so1_manejador_Nulo
; RPN'ized expression: "so1_manejador_Nulo "
; Expanded expression: "so1_manejador_Nulo "
section .relod
	dd	L10
section .data
L10:
	dd	_so1_manejador_Nulo
; RPN'ized expression: "so1_manejador_Nulo "
; Expanded expression: "so1_manejador_Nulo "
section .relod
	dd	L11
section .data
L11:
	dd	_so1_manejador_Nulo
; RPN'ized expression: "so1_manejador_Nulo "
; Expanded expression: "so1_manejador_Nulo "
section .relod
	dd	L12
section .data
L12:
	dd	_so1_manejador_Nulo
; RPN'ized expression: "so1_manejador_Nulo "
; Expanded expression: "so1_manejador_Nulo "
section .relod
	dd	L13
section .data
L13:
	dd	_so1_manejador_Nulo
; RPN'ized expression: "so1_manejador_Nulo "
; Expanded expression: "so1_manejador_Nulo "
section .relod
	dd	L14
section .data
L14:
	dd	_so1_manejador_Nulo
; RPN'ized expression: "so1_manejador_0b "
; Expanded expression: "so1_manejador_0b "
section .relod
	dd	L15
section .data
L15:
	dd	_so1_manejador_0b
; RPN'ized expression: "so1_manejador_Nulo "
; Expanded expression: "so1_manejador_Nulo "
section .relod
	dd	L16
section .data
L16:
	dd	_so1_manejador_Nulo
; RPN'ized expression: "so1_manejador_Nulo "
; Expanded expression: "so1_manejador_Nulo "
section .relod
	dd	L17
section .data
L17:
	dd	_so1_manejador_Nulo
; RPN'ized expression: "so1_manejador_Nulo "
; Expanded expression: "so1_manejador_Nulo "
section .relod
	dd	L18
section .data
L18:
	dd	_so1_manejador_Nulo
; RPN'ized expression: "so1_manejador_Nulo "
; Expanded expression: "so1_manejador_Nulo "
section .relod
	dd	L19
section .data
L19:
	dd	_so1_manejador_Nulo

; glb isr_SO1H : (void) void
section .text
	global	_isr_SO1H
_isr_SO1H:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
; if
; RPN'ized expression: "nivelActivacionSO1H 1 > "
; Expanded expression: "nivelActivacionSO1H *(4) 1 > "
; Fused expression:    "nivelActivacionSO1H > *ax 1 IF! "
section .relod
	dd	L24
section .text
	db	0x66, 0xB8
L24:
	dd	_nivelActivacionSO1H
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	cmp	eax, 1
	jle	L22
; {

section .rodata
L25:
	db	10," llamada al sistema desde el nivel "
	times	1 db 0

section .text
; RPN'ized expression: "( L25 printStrVideo ) "
; Expanded expression: " L25  printStrVideo ()4 "
; Fused expression:    "( L25 , printStrVideo )4 "
section .relod
	dd	L26
section .text
	db	0x66, 0x68
L26:
	dd	L25
	db	0x9A
section .relot
	dd	L27
section .text
L27:
	dd	_printStrVideo
	sub	sp, -4
; RPN'ized expression: "( 1 , nivelActivacionSO1H 1 - printDecVideo ) "
; Expanded expression: " 1  nivelActivacionSO1H *(4) 1 -  printDecVideo ()8 "
; Fused expression:    "( 1 , nivelActivacionSO1H - *ax 1 , printDecVideo )8 "
	push	dword 1
section .relod
	dd	L28
section .text
	db	0x66, 0xB8
L28:
	dd	_nivelActivacionSO1H
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	dec	eax
	push	eax
	db	0x9A
section .relot
	dd	L29
section .text
L29:
	dd	_printDecVideo
	sub	sp, -8

section .rodata
L30:
	db	" (0: usuario, 1: tarea) pindx = "
	times	1 db 0

section .text
; RPN'ized expression: "( L30 printStrVideo ) "
; Expanded expression: " L30  printStrVideo ()4 "
; Fused expression:    "( L30 , printStrVideo )4 "
section .relod
	dd	L31
section .text
	db	0x66, 0x68
L31:
	dd	L30
	db	0x9A
section .relot
	dd	L32
section .text
L32:
	dd	_printStrVideo
	sub	sp, -4
; RPN'ized expression: "( 1 , indProcesoActual printIntVideo ) "
; Expanded expression: " 1  indProcesoActual *(4)  printIntVideo ()8 "
; Fused expression:    "( 1 , indProcesoActual *(4) ax , printIntVideo )8 "
	push	dword 1
section .relod
	dd	L33
section .text
	db	0x66, 0xB8
L33:
	dd	_indProcesoActual
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	push	dword [si]
	db	0x9A
section .relot
	dd	L34
section .text
L34:
	dd	_printIntVideo
	sub	sp, -8

section .rodata
L35:
	db	" tid = "
	times	1 db 0

section .text
; RPN'ized expression: "( L35 printStrVideo ) "
; Expanded expression: " L35  printStrVideo ()4 "
; Fused expression:    "( L35 , printStrVideo )4 "
section .relod
	dd	L36
section .text
	db	0x66, 0x68
L36:
	dd	L35
	db	0x9A
section .relot
	dd	L37
section .text
L37:
	dd	_printStrVideo
	sub	sp, -4
; RPN'ized expression: "( 1 , indThreadActual printIntVideo ) "
; Expanded expression: " 1  indThreadActual *(4)  printIntVideo ()8 "
; Fused expression:    "( 1 , indThreadActual *(4) ax , printIntVideo )8 "
	push	dword 1
section .relod
	dd	L38
section .text
	db	0x66, 0xB8
L38:
	dd	_indThreadActual
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	push	dword [si]
	db	0x9A
section .relot
	dd	L39
section .text
L39:
	dd	_printIntVideo
	sub	sp, -8

section .rodata
L40:
	db	" AX = "
	times	1 db 0

section .text
; RPN'ized expression: "( L40 printStrVideo ) "
; Expanded expression: " L40  printStrVideo ()4 "
; Fused expression:    "( L40 , printStrVideo )4 "
section .relod
	dd	L41
section .text
	db	0x66, 0x68
L41:
	dd	L40
	db	0x9A
section .relot
	dd	L42
section .text
L42:
	dd	_printStrVideo
	sub	sp, -4
; RPN'ized expression: "( 4 , tramaThread tw -> *u &u AX -> *u printHexVideo ) "
; Expanded expression: " 4  tramaThread *(4) 0 + 32 + *(2)  printHexVideo ()8 "
; Fused expression:    "( 4 , tramaThread + *ax 0 + ax 32 *(2) ax , printHexVideo )8 "
	push	dword 4
section .relod
	dd	L43
section .text
	db	0x66, 0xB8
L43:
	dd	_tramaThread
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 32
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	push	eax
	db	0x9A
section .relot
	dd	L44
section .text
L44:
	dd	_printHexVideo
	sub	sp, -8
cli
hlt
; }
L22:
; loc     <something> : * union <something>
; loc     <something> : unsigned
; loc     <something> : unsigned
; RPN'ized expression: "tramaThread SS_Thread (something46) 4 << SP_Thread (something47) + (something45) = "
; Expanded expression: "tramaThread SS_Thread *(2) 4 << SP_Thread *(2) + =(4) "
; Fused expression:    "tramaThread push-ax SS_Thread << *ax 4 push-ax SP_Thread + *sp *ax =(204) **sp ax "
section .relod
	dd	L48
section .text
	db	0x66, 0xB8
L48:
	dd	_tramaThread
	push	eax
section .relod
	dd	L49
section .text
	db	0x66, 0xB8
L49:
	dd	_SS_Thread
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	shl	eax, 4
	push	eax
section .relod
	dd	L50
section .text
	db	0x66, 0xB8
L50:
	dd	_SP_Thread
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
	mov	[si], eax
; loc     51 : unsigned char

section .bss
L51:
	resb	1

section .text
; RPN'ized expression: "codOp tramaThread tb -> *u &u AH -> *u = "
; Expanded expression: "L51 tramaThread *(4) 0 + 33 + *(1) =(1) "
; Fused expression:    "L51 push-ax tramaThread + *ax 0 + ax 33 =(153) **sp *ax "
section .relod
	dd	L52
section .text
	db	0x66, 0xB8
L52:
	dd	L51
	push	eax
section .relod
	dd	L53
section .text
	db	0x66, 0xB8
L53:
	dd	_tramaThread
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 33
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	al, [si]
	movzx	eax, al
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], al
	movzx	eax, al
; if
; loc     <something> : * (void) void
; RPN'ized expression: "codOp manejador sizeof <something56> sizeof / < "
; Expanded expression: "L51 *(1) 16u <u "
; Fused expression:    "L51 <u *ax 16u IF! "
section .relod
	dd	L57
section .text
	db	0x66, 0xB8
L57:
	dd	L51
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	al, [si]
	movzx	eax, al
	cmp	eax, 16
	jae	L54
; RPN'ized expression: "( manejador codOp + *u ) "
; Expanded expression: " manejador L51 *(1) 4 * + *(4) ()0 "
; Fused expression:    "( manejador push-ax L51 * *ax 4 + *sp ax *(4) ax )0 "
section .relod
	dd	L58
section .text
	db	0x66, 0xB8
L58:
	dd	_manejador
	push	eax
section .relod
	dd	L59
section .text
	db	0x66, 0xB8
L59:
	dd	L51
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	al, [si]
	movzx	eax, al
	imul	eax, eax, 4
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	db	0x9A
section .relot
	dd	L60
section .text
L60:
	dd	L61
L61:
	mov	si, sp
	add	word [ss:si], L62 - L61
	shl	eax, 12
	rol	ax, 4
	push	eax
	retf
L62:
L54:
L20:
	db	0x66
	leave
	retf
L63:

section .fxnsz
	dd	L63 - _isr_SO1H


	extern	_so1_manejador_00
	extern	_so1_manejador_01
	extern	_so1_manejador_02
	extern	_so1_manejador_03
	extern	_so1_manejador_0b
	extern	_nivelActivacionSO1H
	extern	_printStrVideo
	extern	_printDecVideo
	extern	_indProcesoActual
	extern	_printIntVideo
	extern	_indThreadActual
	extern	_tramaThread
	extern	_printHexVideo
	extern	_SS_Thread
	extern	_SP_Thread

; Syntax/declaration table/stack:
; Bytes used: 11710/40960


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
; Ident ptrTVI
; Ident valorIMR
; Ident establecerIMR
; Ident nuevoIMR
; Ident mask_pic1
; Ident irq
; Ident mask_pic2
; Ident unmask_pic1
; Ident unmask_pic2
; Ident get_pic1_isr
; Ident get_pic2_isr
; Ident set_pics
; Ident irq0
; Ident irq8
; Ident pic_setup
; Ident enable_hwirq
; Ident hwirq
; Ident rti
; Ident startBin
; Ident modoSO1
; Ident unidadBIOS
; Ident CS_SO1H
; Ident RO_SO1H
; Ident DS_SO1H
; Ident BSS_SO1H
; Ident SS_SO1H
; Ident SS_Kernel
; Ident IMRInicial
; Ident obtenerMapa
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
; Ident descProcesoExt_t
; Ident descThreadExt_t
; Ident descProceso
; Ident descThread
; Ident descFichero
; Ident descRecurso
; Ident c2cPFR
; Ident e2PFR
; Ident descCcbAlEpilogo
; Ident ccbAlEpilogo
; Ident tramaThread
; Ident tramaTarea
; Ident indThreadActual
; Ident indProcesoActual
; Ident indThreadDeSuperficie
; Ident contRodajas
; Ident contTicsRodaja
; Ident contadorTimer00
; Ident contOcioso
; Ident nuevoPid
; Ident nuevoTid
; Ident indice
; Ident registrarEnPOrdenados
; Ident crearThread
; Ident funcion
; Ident crearProceso
; Ident tamFich
; Ident inicProcesos
; Ident resetPids
; Ident resetTids
; Ident terminarThreadIndx
; Ident eliminarThreadIndx
; Ident terminarProcIndx
; Ident eliminarProcIndx
; Ident matarThreadIndx
; Ident matarProcIndx
; Ident link_procs
; Ident SS_Thread
; Ident SP_Thread
; Ident SS_Tarea
; Ident SP_Tarea
; Ident nivelActivacionSO1H
; Ident nVIntActual
; Ident enHalt
; Ident activarAlEpilogo1
; Ident hayTic
; Ident setKernelStack
; Ident setThreadStack
; Ident reg_DL
; Ident prepararDesbloqueadosUrgentes
; Ident printCarVideo
; Ident car
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
; Ident isr_SO1H
; Ident link_llamadas
; Ident so1_manejador_00
; Ident so1_manejador_01
; Ident so1_manejador_02
; Ident so1_manejador_03
; Ident so1_manejador_0b
; Ident so1_manejador_Nulo
; Ident manejador
; Bytes used: 4885/16384

; Next label number: 64
; Compilation succeeded.
