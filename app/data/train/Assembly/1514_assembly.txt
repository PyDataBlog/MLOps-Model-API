section .text
    global _start   ;must be declared for linker (ld)
_start:
    mov eax,11             ; execve syscall
    mov ebx,program        ; name of program
    lea ecx,[esp+4]        ; pointer to argument array
    mov ebp,[esp]          ; number of arguments
    lea edx,[esp+4*ebp+8]  ; pointer to environ array (skip argc and NULL)
    int 0x80
section .data
    program db '/bin/echo',0