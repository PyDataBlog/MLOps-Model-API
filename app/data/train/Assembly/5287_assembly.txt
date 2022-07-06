.MODEL SMALL
.STACK 100H
.DATA

MSG1 DB 'Enter Three Initials: $'
A DB ?
B DB ?
C DB ?

.CODE
MAIN PROC

    MOV AX,@DATA
    MOV DS,AX 
    
    ;PRINT MSG1
    MOV AH,9
    LEA DX,MSG1
    INT 21H
    
    ;INPUT A,B,C   
    MOV AH,1
    INT 21H     
    MOV A,AL
    INT 21H     
    MOV B,AL
    INT 21H     
    MOV C,AL
        
    ;PRINT NEWLINE
    MOV AH,2
    MOV DL,10
    INT 21H
    MOV DL,13
    INT 21H
    
    ;PRINT A 
    MOV AH,2
    MOV DL,A
    INT 21H
    
    ;PRINT NEWLINE
    MOV AH,2
    MOV DL,10
    INT 21H
    MOV DL,13
    INT 21H
    
    ;PRINT B 
    MOV AH,2
    MOV DL,B
    INT 21H 
    
    ;PRINT NEWLINE
    MOV AH,2
    MOV DL,10
    INT 21H
    MOV DL,13
    INT 21H
    
    ;PRINT C 
    MOV AH,2
    MOV DL,C
    INT 21H
   
    
    MOV AH,4CH
    INT 21H
    MAIN ENDP
END MAIN