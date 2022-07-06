ORG 100h
  MOV AL,1
  MOV BL,2 
  
  CALL m2
  CALL m2
  CALL m2
  CALL m2 
  
  RET 
             ; RETURN TO OPERATING SYSTEM.
m2 PROC  
    
  MUL BL  ;AX=AL*BL
  RET
              ; RETURN TO CALLER
m2 ENDP

END