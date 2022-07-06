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
; glb _start__text : char
; glb _stop__text : char
; glb _start__rodata : char
; glb _stop__rodata : char
; glb _start__data : char
; glb _stop__data : char
; glb _start__bss : char
; glb _stop__bss : char
; glb _start_allcode__ : char
; glb _stop_allcode__ : char
; glb _start_alldata__ : char
; glb _stop_alldata__ : char
; glb mostrarSeccion : (
; prm     start : unsigned
; prm     stop : unsigned
; prm     descripcion : * char
;     ) void
; glb mostrarSecciones : (void) void
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
%define SP0_SO1H 0x8000
%define SP0_Kernel  0xA000
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
; RPN'ized expression: "256 "
; Expanded expression: "256 "
; Expression value: 256
; glb VIOrg : [256u] * (void) void
; RPN'ized expression: "256 "
; Expanded expression: "256 "
; Expression value: 256
; glb recVInt : [256u] int
; glb redirigirInt : (
; prm     nVInt : unsigned char
; prm     isr : * (void) void
;     ) void
; glb redirigirIntHardware : (
; prm     irq : unsigned char
; prm     isr : * (void) void
;     ) void
; glb restablecerInt : (
; prm     nVInt : int
;     ) void
; glb inicTVI : (void) void
; glb link_ints : (void) void
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
 section .text       
   global _link_ints 
 _link_ints:         

; RPN'ized expression: "256 "
; Expanded expression: "256 "
; Expression value: 256
; glb VIOrg : [256u] * (void) void
section .bss
	alignb 4
	global	_VIOrg
_VIOrg:
	resb	1024

; RPN'ized expression: "256 "
; Expanded expression: "256 "
; Expression value: 256
; glb recVInt : [256u] int
section .bss
	alignb 4
	global	_recVInt
_recVInt:
	resb	1024

; glb isrNula : (void) void
section .text
	global	_isrNula
_isrNula:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
L1:
	db	0x66
	leave
	retf
L3:

section .fxnsz noalloc
	dd	L3 - _isrNula

; RPN'ized expression: "256 "
; Expanded expression: "256 "
; Expression value: 256
; glb isr : [256u] * (void) void
section .bss
	alignb 4
	global	_isr
_isr:
	resb	1024

%macro RTI 2         
       pushad        
       mov dl,0%1%2h 
       jmp word fin  
%endmacro            

%macro RTIF 1                                     
 RTI %1, 0 
 RTI %1, 1 
 RTI %1, 2 
 RTI %1, 3 
 RTI %1, 4 
 RTI %1, 5 
 RTI %1, 6 
 RTI %1, 7 
 RTI %1, 8 
 RTI %1, 9 
 RTI %1, a 
 RTI %1, b 
 RTI %1, c 
 RTI %1, d 
 RTI %1, e 
 RTI %1, f 
%endmacro                                         

; glb rti_00 : (void) void
; glb envolvente_00 : (void) void
section .text
	global	_envolvente_00
_envolvente_00:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
; rti_00_entry:
L6:
 section .text                        
   global _rti_00                     
 _rti_00:                             
 RTIF 0 
 RTIF 1 
 RTIF 2 
 RTIF 3 
 RTIF 4 
 RTIF 5 
 RTIF 6 
 RTIF 7 
 RTIF 8 
 RTIF 9 
 RTIF a 
 RTIF b 
 RTIF c 
 RTIF d 
 RTIF e 
 RTIF f 
fin:                                  

   push es 
   push ds 

; if
; RPN'ized expression: "nivelActivacionSO1H ++ 1 == "
; Expanded expression: "nivelActivacionSO1H ++(4) 1 == "
; Fused expression:    "nivelActivacionSO1H ++(4) *ax == ax 1 IF! "
section .relod
	dd	L9
section .text
	db	0x66, 0xB8
L9:
	dd	_nivelActivacionSO1H
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	inc	dword [si]
	mov	eax, [si]
	cmp	eax, 1
	jne	L7
; {
   push esp     
   mov ax,ss    
   movzx eax,ax 
   push eax     

; RPN'ized expression: "( setKernelStack ) "
; Expanded expression: " setKernelStack ()0 "
; Fused expression:    "( setKernelStack )0 "
	db	0x9A
section .relot
	dd	L10
section .text
L10:
	dd	_setKernelStack
; }
L7:
; RPN'ized expression: "nVIntActual ( reg_DL ) = "
; Expanded expression: "nVIntActual  reg_DL ()0 =(4) "
; Fused expression:    "nVIntActual push-ax ( reg_DL )0 =(204) **sp ax "
section .relod
	dd	L11
section .text
	db	0x66, 0xB8
L11:
	dd	_nVIntActual
	push	eax
	db	0x9A
section .relot
	dd	L12
section .text
L12:
	dd	_reg_DL
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "( isr nVIntActual + *u ) "
; Expanded expression: " isr nVIntActual *(4) 4 * + *(4) ()0 "
; Fused expression:    "( isr push-ax nVIntActual * *ax 4 + *sp ax *(4) ax )0 "
section .relod
	dd	L13
section .text
	db	0x66, 0xB8
L13:
	dd	_isr
	push	eax
section .relod
	dd	L14
section .text
	db	0x66, 0xB8
L14:
	dd	_nVIntActual
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
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
	dd	L15
section .text
L15:
	dd	L16
L16:
	mov	si, sp
	add	word [ss:si], L17 - L16
	shl	eax, 12
	rol	ax, 4
	push	eax
	retf
L17:
; if
; RPN'ized expression: "nivelActivacionSO1H 1 == enHalt || "
; Expanded expression: "nivelActivacionSO1H *(4) 1 == [sh||->20] enHalt *(4) _Bool ||[20] "
; Fused expression:    "nivelActivacionSO1H == *ax 1 [sh||->20] enHalt *(4) ax _Bool ||[20]  "
section .relod
	dd	L21
section .text
	db	0x66, 0xB8
L21:
	dd	_nivelActivacionSO1H
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	cmp	eax, 1
	sete	al
	movzx	eax, al
; JumpIfNotZero
	test	eax, eax
	jne	L20
section .relod
	dd	L22
section .text
	db	0x66, 0xB8
L22:
	dd	_enHalt
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	test	eax, eax
	setne	al
	movsx	eax, al
L20:
; JumpIfZero
	test	eax, eax
	je	L18
; {
; if
; RPN'ized expression: "ccbAlEpilogo num -> *u 0 > "
; Expanded expression: "ccbAlEpilogo *(4) 0 + *(2) 0 > "
; Fused expression:    "ccbAlEpilogo + *ax 0 > *ax 0 IF! "
section .relod
	dd	L25
section .text
	db	0x66, 0xB8
L25:
	dd	_ccbAlEpilogo
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	cmp	eax, 0
	jle	L23
; RPN'ized expression: "( ccbAlEpilogo atenderCcb ) "
; Expanded expression: " ccbAlEpilogo *(4)  atenderCcb ()4 "
; Fused expression:    "( ccbAlEpilogo *(4) ax , atenderCcb )4 "
section .relod
	dd	L26
section .text
	db	0x66, 0xB8
L26:
	dd	_ccbAlEpilogo
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	push	dword [si]
	db	0x9A
section .relot
	dd	L27
section .text
L27:
	dd	_atenderCcb
	sub	sp, -4
L23:
; if
; RPN'ized expression: "c2cPFR TUrgentes + *u &u numElem -> *u 0 > "
; Expanded expression: "c2cPFR 80 + 0 + *(4) 0 > "
; Fused expression:    "c2cPFR + ax 80 + ax 0 > *ax 0 IF! "
section .relod
	dd	L30
section .text
	db	0x66, 0xB8
L30:
	dd	_c2cPFR
	add	eax, 80
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	cmp	eax, 0
	jle	L28
; RPN'ized expression: "( prepararDesbloqueadosUrgentes ) "
; Expanded expression: " prepararDesbloqueadosUrgentes ()0 "
; Fused expression:    "( prepararDesbloqueadosUrgentes )0 "
	db	0x9A
section .relot
	dd	L31
section .text
L31:
	dd	_prepararDesbloqueadosUrgentes
L28:
; if
; RPN'ized expression: "activarAlEpilogo1 "
; Expanded expression: "activarAlEpilogo1 *(4) "
; Fused expression:    "activarAlEpilogo1 *(4) ax  "
section .relod
	dd	L34
section .text
	db	0x66, 0xB8
L34:
	dd	_activarAlEpilogo1
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
; JumpIfZero
	test	eax, eax
	je	L32
; {
; RPN'ized expression: "activarAlEpilogo1 FALSE = "
; Expanded expression: "activarAlEpilogo1 0 =(4) "
; Fused expression:    "activarAlEpilogo1 =(204) *ax 0 "
section .relod
	dd	L35
section .text
	db	0x66, 0xB8
L35:
	dd	_activarAlEpilogo1
	mov	ebx, eax
	mov	eax, 0
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "( ( sigThread ) activarThread ) "
; Expanded expression: "  sigThread ()0  activarThread ()4 "
; Fused expression:    "( ( sigThread )0 , activarThread )4 "
	db	0x9A
section .relot
	dd	L36
section .text
L36:
	dd	_sigThread
	push	eax
	db	0x9A
section .relot
	dd	L37
section .text
L37:
	dd	_activarThread
	sub	sp, -4
; }
L32:
; }
L18:
; if
; RPN'ized expression: "nivelActivacionSO1H --p 1 == "
; Expanded expression: "nivelActivacionSO1H --p(4) 1 == "
; Fused expression:    "nivelActivacionSO1H --p(4) *ax == ax 1 IF! "
section .relod
	dd	L40
section .text
	db	0x66, 0xB8
L40:
	dd	_nivelActivacionSO1H
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	dec	dword [si]
	cmp	eax, 1
	jne	L38
; {
; RPN'ized expression: "( SP_Thread , SS_Thread setThreadStack ) "
; Expanded expression: " SP_Thread *(2)  SS_Thread *(2)  setThreadStack ()8 "
; Fused expression:    "( SP_Thread *(2) ax , SS_Thread *(2) ax , setThreadStack )8 "
section .relod
	dd	L41
section .text
	db	0x66, 0xB8
L41:
	dd	_SP_Thread
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	push	eax
section .relod
	dd	L42
section .text
	db	0x66, 0xB8
L42:
	dd	_SS_Thread
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
	dd	L43
section .text
L43:
	dd	_setThreadStack
	sub	sp, -8
; }
L38:
   pop ds 
   pop es 
   popad  
   iret   

L4:
	db	0x66
	leave
	retf
L44:

section .fxnsz
	dd	L44 - _envolvente_00

; glb inicTVI : (void) void
section .text
	global	_inicTVI
_inicTVI:
	push	ebp
	movzx	ebp, sp
	 sub	sp,          4
; loc     nVInt : (@-4): int
; for
; RPN'ized expression: "nVInt 0 = "
; Expanded expression: "(@-4) 0 =(4) "
; Fused expression:    "=(204) *(@-4) 0 "
	mov	eax, 0
	mov	[bp-4], eax
L47:
; RPN'ized expression: "nVInt 256 < "
; Expanded expression: "(@-4) *(4) 256 < "
; Fused expression:    "< *(@-4) 256 IF! "
	mov	eax, [bp-4]
	cmp	eax, 256
	jge	L50
; RPN'ized expression: "nVInt ++p "
; Expanded expression: "(@-4) ++p(4) "
; {
; RPN'ized expression: "VIOrg nVInt + *u ptrTVI nVInt + *u = "
; Expanded expression: "VIOrg (@-4) *(4) 4 * + ptrTVI *(4) (@-4) *(4) 4 * + *(4) =(4) "
; Fused expression:    "VIOrg push-ax * *(@-4) 4 + *sp ax push-ax ptrTVI push-ax * *(@-4) 4 + **sp ax =(204) **sp *ax "
section .relod
	dd	L51
section .text
	db	0x66, 0xB8
L51:
	dd	_VIOrg
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 4
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	push	eax
section .relod
	dd	L52
section .text
	db	0x66, 0xB8
L52:
	dd	_ptrTVI
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 4
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
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
; RPN'ized expression: "recVInt nVInt + *u 1 -u = "
; Expanded expression: "recVInt (@-4) *(4) 4 * + -1 =(4) "
; Fused expression:    "recVInt push-ax * *(@-4) 4 + *sp ax =(204) *ax -1 "
section .relod
	dd	L53
section .text
	db	0x66, 0xB8
L53:
	dd	_recVInt
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 4
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; loc         <something> : * (void) void
; RPN'ized expression: "isr nVInt + *u isrNula (something54) = "
; Expanded expression: "isr (@-4) *(4) 4 * + isrNula =(4) "
; Fused expression:    "isr push-ax * *(@-4) 4 + *sp ax push-ax isrNula =(204) **sp ax "
section .relod
	dd	L55
section .text
	db	0x66, 0xB8
L55:
	dd	_isr
	push	eax
	mov	eax, [bp-4]
	imul	eax, eax, 4
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	push	eax
section .relod
	dd	L56
section .text
	db	0x66, 0xB8
L56:
	dd	_isrNula
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; }
L48:
; Fused expression:    "++p(4) *(@-4) "
	mov	eax, [bp-4]
	inc	dword [bp-4]
	jmp	L47
L50:
L45:
	db	0x66
	leave
	retf
L57:

section .fxnsz
	dd	L57 - _inicTVI

; glb redirigirInt : (
; prm     nVInt : unsigned char
; prm     isr_x : * (void) void
;     ) void
section .text
	global	_redirigirInt
_redirigirInt:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
; loc     nVInt : (@8): unsigned char
; loc     isr_x : (@12): * (void) void
; loc     <something> : * (void) void
; loc     <something> : unsigned
; loc     <something> : unsigned short
; loc     <something> : unsigned
; loc     <something> : unsigned short
; loc     <something> : unsigned
; loc     <something> : unsigned
; loc     <something> : unsigned
; RPN'ized expression: "ptrTVI nVInt + *u _start__text &u (something63) 4 >> (something62) (something61) 16 << envolvente_00 &u (something65) 6 + nVInt 7 * (something66) + _start__text &u (something67) - (something64) | (something60) = "
; Expanded expression: "ptrTVI *(4) (@8) *(1) 4 * + _start__text 4 >>u unsigned short 16 << envolvente_00 6 + (@8) *(1) 7 * + _start__text - unsigned short | =(4) "
; Fused expression:    "ptrTVI push-ax * *(@8) 4 + **sp ax push-ax _start__text >>u ax 4 unsigned short << ax 16 push-ax envolvente_00 + ax 6 push-ax * *(@8) 7 + *sp ax push-ax _start__text - *sp ax unsigned short | *sp ax =(204) **sp ax "
section .relod
	dd	L68
section .text
	db	0x66, 0xB8
L68:
	dd	_ptrTVI
	push	eax
	mov	al, [bp+8]
	movzx	eax, al
	imul	eax, eax, 4
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	push	eax
section .relod
	dd	L69
section .text
	db	0x66, 0xB8
L69:
	dd	__start__text
	shr	eax, 4
	movzx	eax, ax
	shl	eax, 16
	push	eax
section .relod
	dd	L70
section .text
	db	0x66, 0xB8
L70:
	dd	_envolvente_00
	add	eax, 6
	push	eax
	mov	al, [bp+8]
	movzx	eax, al
	imul	eax, eax, 7
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	push	eax
section .relod
	dd	L71
section .text
	db	0x66, 0xB8
L71:
	dd	__start__text
	mov	ecx, eax
	pop	eax
	sub	eax, ecx
	movzx	eax, ax
	mov	ecx, eax
	pop	eax
	or	eax, ecx
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "isr nVInt + *u isr_x = "
; Expanded expression: "isr (@8) *(1) 4 * + (@12) *(4) =(4) "
; Fused expression:    "isr push-ax * *(@8) 4 + *sp ax =(204) *ax *(@12) "
section .relod
	dd	L72
section .text
	db	0x66, 0xB8
L72:
	dd	_isr
	push	eax
	mov	al, [bp+8]
	movzx	eax, al
	imul	eax, eax, 4
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	mov	ebx, eax
	mov	eax, [bp+12]
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
L58:
	db	0x66
	leave
	retf
L73:

section .fxnsz
	dd	L73 - _redirigirInt

; glb redirigirIntHardware : (
; prm     irq : unsigned char
; prm     isr : * (void) void
;     ) void
section .text
	global	_redirigirIntHardware
_redirigirIntHardware:
	push	ebp
	movzx	ebp, sp
	 sub	sp,          4
; loc     irq : (@8): unsigned char
; loc     isr : (@12): * (void) void
; loc     nVInt : (@-4): unsigned char
; if
; RPN'ized expression: "irq 8 < "
; Expanded expression: "(@8) *(1) 8 < "
; Fused expression:    "< *(@8) 8 IF! "
	mov	al, [bp+8]
	movzx	eax, al
	cmp	eax, 8
	jge	L76
; {
; RPN'ized expression: "( 1 irq << unmask_pic1 ) "
; Expanded expression: " 1 (@8) *(1) <<  unmask_pic1 ()4 "
; Fused expression:    "( << 1 *(@8) , unmask_pic1 )4 "
	mov	eax, 1
	mov	cl, [bp+8]
	shl	eax, cl
	push	eax
	db	0x9A
section .relot
	dd	L78
section .text
L78:
	dd	_unmask_pic1
	sub	sp, -4
; RPN'ized expression: "nVInt 8 irq + = "
; Expanded expression: "(@-4) 8 (@8) *(1) + =(1) "
; Fused expression:    "+ 8 *(@8) =(156) *(@-4) ax "
	mov	eax, 8
	movzx	ecx, byte [bp+8]
	add	eax, ecx
	mov	[bp-4], al
	movzx	eax, al
; }
	jmp	L77
L76:
; else
; {
; RPN'ized expression: "( 1 2 << unmask_pic1 ) "
; Expanded expression: " 4  unmask_pic1 ()4 "
; Fused expression:    "( 4 , unmask_pic1 )4 "
	push	dword 4
	db	0x9A
section .relot
	dd	L79
section .text
L79:
	dd	_unmask_pic1
	sub	sp, -4
; RPN'ized expression: "( 1 irq 8 - << unmask_pic2 ) "
; Expanded expression: " 1 (@8) *(1) 8 - <<  unmask_pic2 ()4 "
; Fused expression:    "( - *(@8) 8 << 1 ax , unmask_pic2 )4 "
	mov	al, [bp+8]
	movzx	eax, al
	sub	eax, 8
	mov	ecx, eax
	mov	eax, 1
	shl	eax, cl
	push	eax
	db	0x9A
section .relot
	dd	L80
section .text
L80:
	dd	_unmask_pic2
	sub	sp, -4
; RPN'ized expression: "nVInt 112 irq + 8 - = "
; Expanded expression: "(@-4) 112 (@8) *(1) + 8 - =(1) "
; Fused expression:    "+ 112 *(@8) - ax 8 =(156) *(@-4) ax "
	mov	eax, 112
	movzx	ecx, byte [bp+8]
	add	eax, ecx
	sub	eax, 8
	mov	[bp-4], al
	movzx	eax, al
; }
L77:
; RPN'ized expression: "( isr , nVInt redirigirInt ) "
; Expanded expression: " (@12) *(4)  (@-4) *(1)  redirigirInt ()8 "
; Fused expression:    "( *(4) (@12) , *(1) (@-4) , redirigirInt )8 "
	push	dword [bp+12]
	mov	al, [bp-4]
	movzx	eax, al
	push	eax
	db	0x9A
section .relot
	dd	L81
section .text
L81:
	dd	_redirigirInt
	sub	sp, -8
L74:
	db	0x66
	leave
	retf
L82:

section .fxnsz
	dd	L82 - _redirigirIntHardware

; glb restablecerInt : (
; prm     nVInt : int
;     ) void
section .text
	global	_restablecerInt
_restablecerInt:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
; loc     nVInt : (@8): int
; RPN'ized expression: "ptrTVI nVInt + *u VIOrg nVInt + *u = "
; Expanded expression: "ptrTVI *(4) (@8) *(4) 4 * + VIOrg (@8) *(4) 4 * + *(4) =(4) "
; Fused expression:    "ptrTVI push-ax * *(@8) 4 + **sp ax push-ax VIOrg push-ax * *(@8) 4 + *sp ax =(204) **sp *ax "
section .relod
	dd	L85
section .text
	db	0x66, 0xB8
L85:
	dd	_ptrTVI
	push	eax
	mov	eax, [bp+8]
	imul	eax, eax, 4
	mov	ecx, eax
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, ecx
	push	eax
section .relod
	dd	L86
section .text
	db	0x66, 0xB8
L86:
	dd	_VIOrg
	push	eax
	mov	eax, [bp+8]
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
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; loc     <something> : * (void) void
; RPN'ized expression: "isr nVInt + *u isrNula (something87) = "
; Expanded expression: "isr (@8) *(4) 4 * + isrNula =(4) "
; Fused expression:    "isr push-ax * *(@8) 4 + *sp ax push-ax isrNula =(204) **sp ax "
section .relod
	dd	L88
section .text
	db	0x66, 0xB8
L88:
	dd	_isr
	push	eax
	mov	eax, [bp+8]
	imul	eax, eax, 4
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	push	eax
section .relod
	dd	L89
section .text
	db	0x66, 0xB8
L89:
	dd	_isrNula
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
L83:
	db	0x66
	leave
	retf
L90:

section .fxnsz
	dd	L90 - _restablecerInt


	extern	_nivelActivacionSO1H
	extern	_setKernelStack
	extern	_nVIntActual
	extern	_reg_DL
	extern	_enHalt
	extern	_ccbAlEpilogo
	extern	_atenderCcb
	extern	_c2cPFR
	extern	_prepararDesbloqueadosUrgentes
	extern	_activarAlEpilogo1
	extern	_sigThread
	extern	_activarThread
	extern	_SP_Thread
	extern	_SS_Thread
	extern	_setThreadStack
	extern	_ptrTVI
	extern	__start__text
	extern	_unmask_pic1
	extern	_unmask_pic2

; Syntax/declaration table/stack:
; Bytes used: 12295/40960


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
; Ident _start__text
; Ident _stop__text
; Ident _start__rodata
; Ident _stop__rodata
; Ident _start__data
; Ident _stop__data
; Ident _start__bss
; Ident _stop__bss
; Ident _start_allcode__
; Ident _stop_allcode__
; Ident _start_alldata__
; Ident _stop_alldata__
; Ident mostrarSeccion
; Ident start
; Ident stop
; Ident descripcion
; Ident mostrarSecciones
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
; Ident startBin
; Ident unidadBIOS
; Ident CS_SO1H
; Ident RO_SO1H
; Ident DS_SO1H
; Ident BSS_SO1H
; Ident SS_SO1H
; Ident SS_Kernel
; Ident IMRInicial
; Ident obtenerMapa
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
; Ident VIOrg
; Ident recVInt
; Ident redirigirInt
; Ident redirigirIntHardware
; Ident restablecerInt
; Ident inicTVI
; Ident link_ints
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
; Ident isrNula
; Ident rti_00
; Ident envolvente_00
; Ident isr_x
; Bytes used: 5106/16384

; Next label number: 91
; Compilation succeeded.
