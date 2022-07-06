; Moving around 8 bit and 16 bit subsections of eax, ebx, and ecx

SECTION .data
SECTION .text

  global _start

_start:
  nop

  mov ax,067FEh
  mov bx,ax
  mov cl,bh
  mov ch,bl
  xchg cl,ch

  mov eax,1		; Code for Exit Syscall
  mov ebx,0		; Return a code of zero	
  int 80H		; Make kernel call

SECTION .bss

