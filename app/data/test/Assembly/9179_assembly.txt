;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (C) 2014 Qwerty01 (qw3rty01@gmail.com, http://github.com/qwerty01) ;
;                                                                              ;
; This program is free software; you can redistribute it and/or modify it      ;
; under the terms of the GNU General Public License as published by the Free   ;
; Software Foundation; either version 2 of the License, or (at your option)    ;
; any later version.                                                           ;
;                                                                              ;
; This program is distributed in the hope that it will be useful, but WITHOUT  ;
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        ;
; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for    ;
; more details.                                                                ;
;                                                                              ;
; You should have received a copy of the GNU General Public License along with ;
; this program; if not, write to the Free Software Foundation, Inc.,           ;
; 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[SECTION .data]
;;; Here we declare initialized data. For example: messages, prompts,
;;; and numbers that we know in advance

max:		dd	1000
msg:		db 	"%d",10,0

[SECTION .bss]
;;; Here we declare uninitialized data. We're reserving space (and
;;; potentially associating names with that space) that our code
;;; will use as it executes. Think of these as "global variables"
message:	resb 64

[section .text]
global  _main
extern  _printf
extern	_itoa

_main:
	push 3			; int val = getSum(3)
	call _getSum
	add esp, 4
	mov ebx, eax
	push 5			; val += getSum(5)
	call _getSum
	add esp, 4
	add ebx, eax
	push 15			; val += 
	call _getSum
	add esp, 4
	sub ebx, eax

	push	ebx
	push	msg
	call    _printf
	add     esp, 8
	mov		eax, 0
	ret

_getSum:
	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push edx

	xor edx,edx

	mov eax, [max]
	dec eax
	idiv dword [ebp+8]
	inc eax
	imul eax, [ebp+8]
	push eax

	xor edx,edx

	mov eax, [max]
	dec eax
	idiv dword [ebp+8]
	xor edx,edx
	mov ebx, eax
	pop eax
	imul eax, ebx
	mov ebx, 2
	idiv ebx

	pop edx
	pop ecx
	pop ebx
	mov esp, ebp
	pop ebp
	ret