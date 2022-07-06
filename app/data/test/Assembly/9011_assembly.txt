;*************************************************************************************************   
;                                                                                                *
;                                     INSTITUTO POLITECNICO NACIONAL                             *
;                                                                                                *
;                                                                                                *
;                       UNIDAD PROFESIONAL INTERDISCIPLINARIA DE INGENIERIA                      *
;                                                                                                *
;                                Y CIENCIAS SOCIALES Y ADMISTRATIVAS                             *
;                                                                                                *
;                                                                                                *
;                                                                                                *
;                                                                                                *
;                                          "GENERADOR DE SERIES PLUS"                            *
;                                                                                                *
;                                                                                                *
;                                                                                                *
;                                              PROGRAMA NO. 16                                   *
;                                                                                                *
;                                                                                                *
;                                                                                                *
;   DESCRIPCION: EL PROGRAMA SOLICITA AL USUARIO EL NUMERO DE POSICIONES QUE OCUPARA LA SERIE    *
;                EN CADA ITERACION SE INCREMENTA EN 3 EL NUMERO QUE SE IMPRIMIRA EN PANTALLA     *
;                ENTONCES LA SERIE VA DE 3 EN 3                                                  *
;                                                                                                *
;                                                                                                *
;                                                                                                *
;                                                                                                *
;                                                                                                *
;                                                                                                *
;*************************************************************************************************

INCLUDE "EMU8086.INC"
;GENERA SERIE 3,6,9...


PRINT "SERIE GENERADA: "
 
 
 CICLO:
 
 CMP BX,20
 JE SALIR 
 
 ADD AX,3 
 ADD BX,1  
 
 
     
     
      
     CALL PRINT_NUM 
      PRINT ", "
     
     
     
     
     CMP AX,15
     JGE COMPARA
     JMP CICLO
     
     
     COMPARA:
        CMP AX,450
        JBE GUARDAR
        JMP CICLO
        
               
        GUARDAR:
            ADD CX,1
            JMP CICLO
            
            
     
     
     
     
     
   
   SALIR:
   
   PRINTN
   PRINTN
   
   PRINT "LA SUMA DE LOS PRIMEROS 200 NUMEROS DE LA SERIE ES: "
   CALL PRINT_NUM
   
   PRINTN
   PRINTN 
   PRINT "HAY "
   MOV AX,CX
   CALL PRINT_NUM
   PRINT " NUMEROS ENTRE 15 Y 450 SEGUN LA SERIE"
   
   RET     
   
   ;---------INCLUIR ESTAS LIBRERIAS PARA QUE JALE-------------- 
  
 DEFINE_SCAN_NUM 
 DEFINE_PRINT_NUM 
 DEFINE_PRINT_NUM_UNS  
  
 ;---------INCLUIR ESTAS LIBRERIAS PARA QUE JALE----------- 

   
 
  