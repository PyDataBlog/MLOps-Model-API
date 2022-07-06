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
; glb kk_fork : (void) int
section .text
	global	_kk_fork
_kk_fork:
	push	ebp
	movzx	ebp, sp
	 sub	sp,         24
; loc     segmento : (@-4): unsigned short
; loc     pindx : (@-8): int
; loc     origen : (@-12): * unsigned char
; loc     destino : (@-16): * unsigned char
; loc     SS_NuevoProceso : (@-20): unsigned short
; loc     tam : (@-24): unsigned short
; RPN'ized expression: "tam descProceso indProcesoActual + *u &u tam -> *u = "
; Expanded expression: "(@-24) descProceso indProcesoActual *(4) 292 * + 60 + *(2) =(2) "
; Fused expression:    "descProceso push-ax indProcesoActual * *ax 292 + *sp ax + ax 60 =(202) *(@-24) *ax "
section .relod
	dd	L3
section .text
	db	0x66, 0xB8
L3:
	dd	_descProceso
	push	eax
section .relod
	dd	L4
section .text
	db	0x66, 0xB8
L4:
	dd	_indProcesoActual
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 60
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	mov	[bp-24], eax
; if
; RPN'ized expression: "c2cPFR DPOcupados + *u &u numElem -> *u 16 1 - >= "
; Expanded expression: "c2cPFR 16 + 0 + *(4) 15 >= "
; Fused expression:    "c2cPFR + ax 16 + ax 0 >= *ax 15 IF! "
section .relod
	dd	L7
section .text
	db	0x66, 0xB8
L7:
	dd	_c2cPFR
	add	eax, 16
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	cmp	eax, 15
	jl	L5
; return
; RPN'ized expression: "1 -u "
; Expanded expression: "-1 "
; Expression value: -1
; Fused expression:    "-1  "
	mov	eax, -1
	jmp	L1
L5:
; if
; RPN'ized expression: "segmento ( tam k_buscarBloque ) = 0 == "
; Expanded expression: "(@-4)  (@-24) *(2)  k_buscarBloque ()4 =(2) 0 == "
; Fused expression:    "( *(2) (@-24) , k_buscarBloque )4 =(172) *(@-4) ax == ax 0 IF! "
	mov	ax, [bp-24]
	movzx	eax, ax
	push	eax
	db	0x9A
section .relot
	dd	L10
section .text
L10:
	dd	_k_buscarBloque
	sub	sp, -4
	mov	[bp-4], ax
	movzx	eax, ax
	cmp	eax, 0
	jne	L8
; return
; RPN'ized expression: "2 -u "
; Expanded expression: "-2 "
; Expression value: -2
; Fused expression:    "-2  "
	mov	eax, -2
	jmp	L1
L8:
; loc     <something> : unsigned
; loc     <something> : unsigned
; RPN'ized expression: "origen descProceso indProcesoActual + *u &u CSProc -> *u (something11) 4 << 0 (something12) + = "
; Expanded expression: "(@-12) descProceso indProcesoActual *(4) 292 * + 52 + *(2) 4 << 0u + =(4) "
; Fused expression:    "descProceso push-ax indProcesoActual * *ax 292 + *sp ax + ax 52 << *ax 4 + ax 0u =(204) *(@-12) ax "
section .relod
	dd	L13
section .text
	db	0x66, 0xB8
L13:
	dd	_descProceso
	push	eax
section .relod
	dd	L14
section .text
	db	0x66, 0xB8
L14:
	dd	_indProcesoActual
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 52
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	ax, [si]
	movzx	eax, ax
	shl	eax, 4
	mov	[bp-12], eax
; loc     <something> : unsigned
; loc     <something> : unsigned
; RPN'ized expression: "destino segmento (something15) 4 << 0 (something16) + = "
; Expanded expression: "(@-16) (@-4) *(2) 4 << 0u + =(4) "
; Fused expression:    "<< *(@-4) 4 + ax 0u =(204) *(@-16) ax "
	mov	ax, [bp-4]
	movzx	eax, ax
	shl	eax, 4
	mov	[bp-16], eax
; loc     <something> : unsigned
; loc     <something> : unsigned
; RPN'ized expression: "( 16 (something17) tam (something18) * , origen , destino memcpy ) "
; Expanded expression: " 16u (@-24) *(2) *  (@-12) *(4)  (@-16) *(4)  memcpy ()12 "
; Fused expression:    "( * 16u *(@-24) , *(4) (@-12) , *(4) (@-16) , memcpy )12 "
	mov	eax, 16
	movzx	ecx, word [bp-24]
	mul	ecx
	push	eax
	push	dword [bp-12]
	push	dword [bp-16]
	db	0x9A
section .relot
	dd	L19
section .text
L19:
	dd	_memcpy
	sub	sp, -12
; loc     <something> : * struct <something>
; RPN'ized expression: "pindx ( c2cPFR DPLibres + *u &u (something20) desencolarPC2c ) = "
; Expanded expression: "(@-8)  c2cPFR 0 +  desencolarPC2c ()4 =(4) "
; Fused expression:    "( c2cPFR + ax 0 , desencolarPC2c )4 =(204) *(@-8) ax "
section .relod
	dd	L21
section .text
	db	0x66, 0xB8
L21:
	dd	_c2cPFR
	push	eax
	db	0x9A
section .relot
	dd	L22
section .text
L22:
	dd	_desencolarPC2c
	sub	sp, -4
	mov	[bp-8], eax
; loc     <something> : * struct <something>
; RPN'ized expression: "( c2cPFR DPOcupados + *u &u (something23) , pindx encolarPC2c ) "
; Expanded expression: " c2cPFR 16 +  (@-8) *(4)  encolarPC2c ()8 "
; Fused expression:    "( c2cPFR + ax 16 , *(4) (@-8) , encolarPC2c )8 "
section .relod
	dd	L24
section .text
	db	0x66, 0xB8
L24:
	dd	_c2cPFR
	add	eax, 16
	push	eax
	push	dword [bp-8]
	db	0x9A
section .relot
	dd	L25
section .text
L25:
	dd	_encolarPC2c
	sub	sp, -8
; loc     <something> : * unsigned char
; RPN'ized expression: "origen descProceso indProcesoActual + *u &u (something26) = "
; Expanded expression: "(@-12) descProceso indProcesoActual *(4) 292 * + =(4) "
; Fused expression:    "descProceso push-ax indProcesoActual * *ax 292 + *sp ax =(204) *(@-12) ax "
section .relod
	dd	L27
section .text
	db	0x66, 0xB8
L27:
	dd	_descProceso
	push	eax
section .relod
	dd	L28
section .text
	db	0x66, 0xB8
L28:
	dd	_indProcesoActual
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	mov	[bp-12], eax
; loc     <something> : * unsigned char
; RPN'ized expression: "destino descProceso pindx + *u &u (something29) = "
; Expanded expression: "(@-16) descProceso (@-8) *(4) 292 * + =(4) "
; Fused expression:    "descProceso push-ax * *(@-8) 292 + *sp ax =(204) *(@-16) ax "
section .relod
	dd	L30
section .text
	db	0x66, 0xB8
L30:
	dd	_descProceso
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	mov	[bp-16], eax
; loc     <something> : struct <something>
; RPN'ized expression: "( <something31> sizeof , origen , destino memcpy ) "
; Expanded expression: " 292u  (@-12) *(4)  (@-16) *(4)  memcpy ()12 "
; Fused expression:    "( 292u , *(4) (@-12) , *(4) (@-16) , memcpy )12 "
	push	dword 292
	push	dword [bp-12]
	push	dword [bp-16]
	db	0x9A
section .relot
	dd	L32
section .text
L32:
	dd	_memcpy
	sub	sp, -12
; loc     <something> : * unsigned char
; RPN'ized expression: "SS_NuevoProceso segmento ( descThread indProcesoActual + *u &u trama -> *u (something33) seg ) descProceso indProcesoActual + *u &u CSProc -> *u - + = "
; Expanded expression: "(@-20) (@-4) *(2)  descThread indProcesoActual *(4) 40 * + 12 + *(4)  seg ()4 descProceso indProcesoActual *(4) 292 * + 52 + *(2) - + =(2) "
; Fused expression:    "( descThread push-ax indProcesoActual * *ax 40 + *sp ax + ax 12 *(4) ax , seg )4 push-ax descProceso push-ax indProcesoActual * *ax 292 + *sp ax + ax 52 - *sp *ax + *(@-4) ax =(172) *(@-20) ax "
section .relod
	dd	L34
section .text
	db	0x66, 0xB8
L34:
	dd	_descThread
	push	eax
section .relod
	dd	L35
section .text
	db	0x66, 0xB8
L35:
	dd	_indProcesoActual
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	push	dword [si]
	db	0x9A
section .relot
	dd	L36
section .text
L36:
	dd	_seg
	sub	sp, -4
	push	eax
section .relod
	dd	L37
section .text
	db	0x66, 0xB8
L37:
	dd	_descProceso
	push	eax
section .relod
	dd	L38
section .text
	db	0x66, 0xB8
L38:
	dd	_indProcesoActual
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 52
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	movzx	ecx, word [si]
	pop	eax
	sub	eax, ecx
	mov	ecx, eax
	mov	ax, [bp-4]
	movzx	eax, ax
	add	eax, ecx
	mov	[bp-20], ax
	movzx	eax, ax
; loc     <something> : * union <something>
; loc     <something> : unsigned
; loc     <something> : unsigned short
; loc     <something> : * unsigned char
; RPN'ized expression: "descThread pindx + *u &u trama -> *u SS_NuevoProceso (something40) 16 << ( descThread indProcesoActual + *u &u trama -> *u (something42) off ) (something41) | (something39) = "
; Expanded expression: "descThread (@-8) *(4) 40 * + 12 + (@-20) *(2) 16 <<  descThread indProcesoActual *(4) 40 * + 12 + *(4)  off ()4 unsigned short | =(4) "
; Fused expression:    "descThread push-ax * *(@-8) 40 + *sp ax + ax 12 push-ax << *(@-20) 16 push-ax ( descThread push-ax indProcesoActual * *ax 40 + *sp ax + ax 12 *(4) ax , off )4 unsigned short | *sp ax =(204) **sp ax "
section .relod
	dd	L43
section .text
	db	0x66, 0xB8
L43:
	dd	_descThread
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 12
	push	eax
	mov	ax, [bp-20]
	movzx	eax, ax
	shl	eax, 16
	push	eax
section .relod
	dd	L44
section .text
	db	0x66, 0xB8
L44:
	dd	_descThread
	push	eax
section .relod
	dd	L45
section .text
	db	0x66, 0xB8
L45:
	dd	_indProcesoActual
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	push	dword [si]
	db	0x9A
section .relot
	dd	L46
section .text
L46:
	dd	_off
	sub	sp, -4
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
; RPN'ized expression: "descProceso pindx + *u &u CSProc -> *u segmento = "
; Expanded expression: "descProceso (@-8) *(4) 292 * + 52 + (@-4) *(2) =(2) "
; Fused expression:    "descProceso push-ax * *(@-8) 292 + *sp ax + ax 52 =(170) *ax *(@-4) "
section .relod
	dd	L47
section .text
	db	0x66, 0xB8
L47:
	dd	_descProceso
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 52
	mov	ebx, eax
	mov	ax, [bp-4]
	movzx	eax, ax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "descThread pindx + *u &u trama -> *u tw -> *u &u CS -> *u segmento = "
; Expanded expression: "descThread (@-8) *(4) 40 * + 12 + *(4) 0 + 38 + (@-4) *(2) =(2) "
; Fused expression:    "descThread push-ax * *(@-8) 40 + *sp ax + ax 12 + *ax 0 + ax 38 =(170) *ax *(@-4) "
section .relod
	dd	L48
section .text
	db	0x66, 0xB8
L48:
	dd	_descThread
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 40
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 12
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	add	eax, 38
	mov	ebx, eax
	mov	ax, [bp-4]
	movzx	eax, ax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], ax
	movzx	eax, ax
; RPN'ized expression: "( pindx registrarEnPOrdenados ) "
; Expanded expression: " (@-8) *(4)  registrarEnPOrdenados ()4 "
; Fused expression:    "( *(4) (@-8) , registrarEnPOrdenados )4 "
	push	dword [bp-8]
	db	0x9A
section .relot
	dd	L49
section .text
L49:
	dd	_registrarEnPOrdenados
	sub	sp, -4
; RPN'ized expression: "descProceso pindx + *u &u pid -> *u ( nuevoPid ) = "
; Expanded expression: "descProceso (@-8) *(4) 292 * + 0 +  nuevoPid ()0 =(4) "
; Fused expression:    "descProceso push-ax * *(@-8) 292 + *sp ax + ax 0 push-ax ( nuevoPid )0 =(204) **sp ax "
section .relod
	dd	L50
section .text
	db	0x66, 0xB8
L50:
	dd	_descProceso
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	push	eax
	db	0x9A
section .relot
	dd	L51
section .text
L51:
	dd	_nuevoPid
	pop	ebx
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; RPN'ized expression: "descProceso pindx + *u &u hpindx -> *u 1 -u = "
; Expanded expression: "descProceso (@-8) *(4) 292 * + 16 + -1 =(4) "
; Fused expression:    "descProceso push-ax * *(@-8) 292 + *sp ax + ax 16 =(204) *ax -1 "
section .relod
	dd	L52
section .text
	db	0x66, 0xB8
L52:
	dd	_descProceso
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 16
	mov	ebx, eax
	mov	eax, -1
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	[si], eax
; loc     <something> : * struct <something>
; loc     <something> : * struct <something>
; RPN'ized expression: "( TRUE , 16 pindx + , e2PFR &u e2Hijos -> *u &u (something54) , descProceso pindx + *u &u c2cHijos -> *u &u (something53) inicializarPC2c ) "
; Expanded expression: " 1  16 (@-8) *(4) +  e2PFR 24816 +  descProceso (@-8) *(4) 292 * + 20 +  inicializarPC2c ()16 "
; Fused expression:    "( 1 , + 16 *(@-8) , e2PFR + ax 24816 , descProceso push-ax * *(@-8) 292 + *sp ax + ax 20 , inicializarPC2c )16 "
	push	dword 1
	mov	eax, 16
	add	eax, [bp-8]
	push	eax
section .relod
	dd	L55
section .text
	db	0x66, 0xB8
L55:
	dd	_e2PFR
	add	eax, 24816
	push	eax
section .relod
	dd	L56
section .text
	db	0x66, 0xB8
L56:
	dd	_descProceso
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 20
	push	eax
	db	0x9A
section .relot
	dd	L57
section .text
L57:
	dd	_inicializarPC2c
	sub	sp, -16
; RPN'ized expression: "descProceso pindx + *u &u ppindx -> *u indProcesoActual = "
; Expanded expression: "descProceso (@-8) *(4) 292 * + 12 + indProcesoActual *(4) =(4) "
; Fused expression:    "descProceso push-ax * *(@-8) 292 + *sp ax + ax 12 push-ax indProcesoActual =(204) **sp *ax "
section .relod
	dd	L58
section .text
	db	0x66, 0xB8
L58:
	dd	_descProceso
	push	eax
	mov	eax, [bp-8]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 12
	push	eax
section .relod
	dd	L59
section .text
	db	0x66, 0xB8
L59:
	dd	_indProcesoActual
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
; loc     <something> : * struct <something>
; RPN'ized expression: "( descProceso indProcesoActual + *u &u c2cHijos -> *u &u (something60) , pindx encolarPC2c ) "
; Expanded expression: " descProceso indProcesoActual *(4) 292 * + 20 +  (@-8) *(4)  encolarPC2c ()8 "
; Fused expression:    "( descProceso push-ax indProcesoActual * *ax 292 + *sp ax + ax 20 , *(4) (@-8) , encolarPC2c )8 "
section .relod
	dd	L61
section .text
	db	0x66, 0xB8
L61:
	dd	_descProceso
	push	eax
section .relod
	dd	L62
section .text
	db	0x66, 0xB8
L62:
	dd	_indProcesoActual
	mov	ebx, eax
	mov	esi, ebx
	ror	esi, 4
	mov	ds, si
	shr	esi, 28
	mov	eax, [si]
	imul	eax, eax, 292
	mov	ecx, eax
	pop	eax
	add	eax, ecx
	add	eax, 20
	push	eax
	push	dword [bp-8]
	db	0x9A
section .relot
	dd	L63
section .text
L63:
	dd	_encolarPC2c
	sub	sp, -8
; return
; RPN'ized expression: "pindx "
; Expanded expression: "(@-8) *(4) "
; Fused expression:    "*(4) (@-8)  "
	mov	eax, [bp-8]
L1:
	db	0x66
	leave
	retf
L64:

section .fxnsz noalloc
	dd	L64 - _kk_fork

; glb preEjecutar : (
; prm     nombre : * char
; prm     comando : * char
; prm     pindx : int
;     ) int
section .text
	global	_preEjecutar
_preEjecutar:
	push	ebp
	movzx	ebp, sp
	;sub	sp,          0
; loc     nombre : (@8): * char
; loc     comando : (@12): * char
; loc     pindx : (@16): int
L65:
	db	0x66
	leave
	retf
L67:

section .fxnsz
	dd	L67 - _preEjecutar


	extern	_descProceso
	extern	_indProcesoActual
	extern	_c2cPFR
	extern	_k_buscarBloque
	extern	_memcpy
	extern	_desencolarPC2c
	extern	_encolarPC2c
	extern	_descThread
	extern	_seg
	extern	_off
	extern	_registrarEnPOrdenados
	extern	_nuevoPid
	extern	_e2PFR
	extern	_inicializarPC2c

; Syntax/declaration table/stack:
; Bytes used: 10970/40960


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
; Ident kk_fork
; Ident preEjecutar
; Bytes used: 4122/16384

; Next label number: 68
; Compilation succeeded.
