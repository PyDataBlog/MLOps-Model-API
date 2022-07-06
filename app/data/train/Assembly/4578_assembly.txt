
;num 14
INCLUDE "EMU8086.INC"  

    MOV DX,65000
    
   
    PRINTN ""
    PRINT " DAME UN NUMERO:  "
    CALL SCAN_NUM   
    PRINTN " "
    PRINTN "  "
     
    CMP CX,DX
    JB MENOR

    CICLO:
    PRINTN ""
    PRINT " DAME UN NUMERO:  "
    CALL SCAN_NUM   
    PRINTN " "
    PRINTN "  "
      
      
    
    CMP CX, 999
    JE EXIT 
    
    CMP CX,AX
    JB MENOR
    
    JMP CICLO
    
    
    MENOR:
    MOV AX,CX
    JMP CICLO
    
    
    EXIT:
    PRINT "EL NUMERO MENOR ES: "
    CALL PRINT_NUM
    RET
    
    DEFINE_SCAN_NUM
    DEFINE_PRINT_NUM
    DEFINE_PRINT_NUM_UNS