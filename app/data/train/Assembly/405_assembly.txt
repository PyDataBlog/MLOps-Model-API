org 00h
mov p1,#0ffh
mov a,#0Eh
rs bit p2.0
rw bit p2.1
en bit p2.2
start:
mov a, #38h
acall command
call delay
call delay
mov a,#0Eh
acall command
call delay
call delay
mov a,#01h
acall command
call delay
call delay
mov a,#06h
acall command
call delay
call delay
mov a,#80h
acall command
call delay
call delay
mov a,#'T'
acall DataRead
call delay
mov a,#'H'
acall DataRead
call delay
mov a,#'I'
acall DataRead
call delay
mov a,#'S'
acall DataRead
call delay
mov a,#14h
acall command
call delay
mov a,#'I'
acall DataRead
call delay
mov a,#'S'
acall DataRead
call delay
mov a,#14h
acall command
call delay
mov a,#'E'
acall DataRead
call delay
mov a,#'L'
acall DataRead
call delay
mov a,#'D'
acall DataRead
call delay
mov a,#'I'
acall DataRead
call delay
mov a,#01h
acall command
call delay
mov a, #0C0h
acall command
call delay
call delay
mov a,#'U'
acall DataRead
call delay
mov a,#14h
acall command
call delay
mov a,#'A'
acall DataRead
call delay
mov a,#'R'
acall DataRead
call delay
mov a,#'E'
acall DataRead
call delay
mov a,#14h
acall command
call delay
mov a,#'W'
acall DataRead
call delay
mov a,#'E'
acall DataRead
call delay
mov a,#'L'
acall DataRead
call delay
mov a,#'C'
acall DataRead
call delay
mov a,#'O'
acall DataRead
call delay
mov a,#'M'
acall DataRead
call delay
mov a,#'E'
acall DataRead
call delay
jmp start
command:
acall ready
mov p1,a
clr p2.0
clr p2.1
setb p2.2
clr p2.2
ret

DataRead:
acall ready
mov p1,a
setb p2.0
clr p2.1
setb p2.2
clr p2.2
ret

ready:
setb p1.7
clr p2.0
setb p2.1
here:
setb p2.2
clr p2.2
Jz here
jnb p1.7,here
ret

delay: mov r0,#255
delay1: mov r1,#255
delay2:nop
djnz r1,delay2
djnz r0,delay1
ret
end
