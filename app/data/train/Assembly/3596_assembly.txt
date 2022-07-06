; Performs division of numbers using repeated subtraction 
jmp start

start: lxi h,0000h
mov a,m
inx h
mov d,m
mvi b,00h
loop: sub d
inr b
cmp d
jnc loop
inx h
mov m,a
inx h
mov m,b
hlt
