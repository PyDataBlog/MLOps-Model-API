;Main.asm
BITS 16
ORG 0x7C00

Main:
jmp 0x0000:start
start:

BootDrive:
db 0
cli

xor ax, ax
mov ds, ax
mov es, ax
mov fs, ax
mov gs, ax
mov ss, ax

mov sp, 0x7C00
mov [BootDrive], dl
jmp $
