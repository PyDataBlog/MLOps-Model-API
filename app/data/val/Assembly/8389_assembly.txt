; 64 bit empty in linux nasm

global _start

section .text
_start:

	mov rax, 60 ; sys_exit
	mov rdi, 0 ; return 0
	syscall


