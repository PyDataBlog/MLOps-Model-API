
_main:

;E_Load_Sim.c,1 :: 		void main()
;E_Load_Sim.c,3 :: 		OSCCON                                             = 0xF0;
	MOVLW      240
	MOVWF      OSCCON+0
;E_Load_Sim.c,5 :: 		TRISA                                              = 0x00;
	CLRF       TRISA+0
;E_Load_Sim.c,6 :: 		LATA                                               = 0x00;
	CLRF       LATA+0
;E_Load_Sim.c,8 :: 		DACCON0                                            = 0xA0;
	MOVLW      160
	MOVWF      DACCON0+0
;E_Load_Sim.c,9 :: 		DACCON1                                            = 127;
	MOVLW      127
	MOVWF      DACCON1+0
;E_Load_Sim.c,11 :: 		while(1)
L_main0:
;E_Load_Sim.c,14 :: 		}
	GOTO       L_main0
;E_Load_Sim.c,15 :: 		}
L_end_main:
	GOTO       $+0
; end of _main
