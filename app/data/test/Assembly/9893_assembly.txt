print_string:
pusha
mov ah,0x0e
mov al,[bx]

p_s_loop:
cmp al,0
je p_s_endp
int 0x10
inc bx
mov al,[bx]
jmp p_s_loop

p_s_endp:
mov al,":"
int 0x10
popa
ret
