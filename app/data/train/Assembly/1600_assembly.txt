;--------------------------------------------------------
; File Created by SDCC : free open source ANSI-C Compiler
; Version 3.0.0 #6037 (Oct 31 2010) (MINGW32)
; This file was generated Mon Dec 10 17:37:56 2012
;--------------------------------------------------------
	.module test_acc
	.optsdcc -mmcs51 --model-medium
	
;--------------------------------------------------------
; Public variables in this module
;--------------------------------------------------------
	.globl _main
	.globl _ISR_ADC
	.globl _ISR_T3
;--------------------------------------------------------
; special function registers
;--------------------------------------------------------
	.area RSEG    (ABS,DATA)
	.org 0x0000
Ftest_acc$P0$0$0 == 0x0080
_P0	=	0x0080
Ftest_acc$SP$0$0 == 0x0081
_SP	=	0x0081
Ftest_acc$DPL0$0$0 == 0x0082
_DPL0	=	0x0082
Ftest_acc$DPH0$0$0 == 0x0083
_DPH0	=	0x0083
Ftest_acc$DPL1$0$0 == 0x0084
_DPL1	=	0x0084
Ftest_acc$DPH1$0$0 == 0x0085
_DPH1	=	0x0085
Ftest_acc$U0CSR$0$0 == 0x0086
_U0CSR	=	0x0086
Ftest_acc$PCON$0$0 == 0x0087
_PCON	=	0x0087
Ftest_acc$TCON$0$0 == 0x0088
_TCON	=	0x0088
Ftest_acc$P0IFG$0$0 == 0x0089
_P0IFG	=	0x0089
Ftest_acc$P1IFG$0$0 == 0x008a
_P1IFG	=	0x008a
Ftest_acc$P2IFG$0$0 == 0x008b
_P2IFG	=	0x008b
Ftest_acc$PICTL$0$0 == 0x008c
_PICTL	=	0x008c
Ftest_acc$P1IEN$0$0 == 0x008d
_P1IEN	=	0x008d
Ftest_acc$P0INP$0$0 == 0x008f
_P0INP	=	0x008f
Ftest_acc$P1$0$0 == 0x0090
_P1	=	0x0090
Ftest_acc$RFIM$0$0 == 0x0091
_RFIM	=	0x0091
Ftest_acc$DPS$0$0 == 0x0092
_DPS	=	0x0092
Ftest_acc$MPAGE$0$0 == 0x0093
_MPAGE	=	0x0093
Ftest_acc$ENDIAN$0$0 == 0x0095
_ENDIAN	=	0x0095
Ftest_acc$S0CON$0$0 == 0x0098
_S0CON	=	0x0098
Ftest_acc$IEN2$0$0 == 0x009a
_IEN2	=	0x009a
Ftest_acc$S1CON$0$0 == 0x009b
_S1CON	=	0x009b
Ftest_acc$T2CT$0$0 == 0x009c
_T2CT	=	0x009c
Ftest_acc$T2PR$0$0 == 0x009d
_T2PR	=	0x009d
Ftest_acc$T2CTL$0$0 == 0x009e
_T2CTL	=	0x009e
Ftest_acc$P2$0$0 == 0x00a0
_P2	=	0x00a0
Ftest_acc$WORIRQ$0$0 == 0x00a1
_WORIRQ	=	0x00a1
Ftest_acc$WORCTRL$0$0 == 0x00a2
_WORCTRL	=	0x00a2
Ftest_acc$WOREVT0$0$0 == 0x00a3
_WOREVT0	=	0x00a3
Ftest_acc$WOREVT1$0$0 == 0x00a4
_WOREVT1	=	0x00a4
Ftest_acc$WORTIME0$0$0 == 0x00a5
_WORTIME0	=	0x00a5
Ftest_acc$WORTIME1$0$0 == 0x00a6
_WORTIME1	=	0x00a6
Ftest_acc$IEN0$0$0 == 0x00a8
_IEN0	=	0x00a8
Ftest_acc$IP0$0$0 == 0x00a9
_IP0	=	0x00a9
Ftest_acc$FWT$0$0 == 0x00ab
_FWT	=	0x00ab
Ftest_acc$FADDRL$0$0 == 0x00ac
_FADDRL	=	0x00ac
Ftest_acc$FADDRH$0$0 == 0x00ad
_FADDRH	=	0x00ad
Ftest_acc$FCTL$0$0 == 0x00ae
_FCTL	=	0x00ae
Ftest_acc$FWDATA$0$0 == 0x00af
_FWDATA	=	0x00af
Ftest_acc$ENCDI$0$0 == 0x00b1
_ENCDI	=	0x00b1
Ftest_acc$ENCDO$0$0 == 0x00b2
_ENCDO	=	0x00b2
Ftest_acc$ENCCS$0$0 == 0x00b3
_ENCCS	=	0x00b3
Ftest_acc$ADCCON1$0$0 == 0x00b4
_ADCCON1	=	0x00b4
Ftest_acc$ADCCON2$0$0 == 0x00b5
_ADCCON2	=	0x00b5
Ftest_acc$ADCCON3$0$0 == 0x00b6
_ADCCON3	=	0x00b6
Ftest_acc$IEN1$0$0 == 0x00b8
_IEN1	=	0x00b8
Ftest_acc$IP1$0$0 == 0x00b9
_IP1	=	0x00b9
Ftest_acc$ADCL$0$0 == 0x00ba
_ADCL	=	0x00ba
Ftest_acc$ADCH$0$0 == 0x00bb
_ADCH	=	0x00bb
Ftest_acc$RNDL$0$0 == 0x00bc
_RNDL	=	0x00bc
Ftest_acc$RNDH$0$0 == 0x00bd
_RNDH	=	0x00bd
Ftest_acc$SLEEP$0$0 == 0x00be
_SLEEP	=	0x00be
Ftest_acc$IRCON$0$0 == 0x00c0
_IRCON	=	0x00c0
Ftest_acc$U0DBUF$0$0 == 0x00c1
_U0DBUF	=	0x00c1
Ftest_acc$U0BAUD$0$0 == 0x00c2
_U0BAUD	=	0x00c2
Ftest_acc$U0UCR$0$0 == 0x00c4
_U0UCR	=	0x00c4
Ftest_acc$U0GCR$0$0 == 0x00c5
_U0GCR	=	0x00c5
Ftest_acc$CLKCON$0$0 == 0x00c6
_CLKCON	=	0x00c6
Ftest_acc$MEMCTR$0$0 == 0x00c7
_MEMCTR	=	0x00c7
Ftest_acc$WDCTL$0$0 == 0x00c9
_WDCTL	=	0x00c9
Ftest_acc$T3CNT$0$0 == 0x00ca
_T3CNT	=	0x00ca
Ftest_acc$T3CTL$0$0 == 0x00cb
_T3CTL	=	0x00cb
Ftest_acc$T3CCTL0$0$0 == 0x00cc
_T3CCTL0	=	0x00cc
Ftest_acc$T3CC0$0$0 == 0x00cd
_T3CC0	=	0x00cd
Ftest_acc$T3CCTL1$0$0 == 0x00ce
_T3CCTL1	=	0x00ce
Ftest_acc$T3CC1$0$0 == 0x00cf
_T3CC1	=	0x00cf
Ftest_acc$PSW$0$0 == 0x00d0
_PSW	=	0x00d0
Ftest_acc$DMAIRQ$0$0 == 0x00d1
_DMAIRQ	=	0x00d1
Ftest_acc$DMA1CFGL$0$0 == 0x00d2
_DMA1CFGL	=	0x00d2
Ftest_acc$DMA1CFGH$0$0 == 0x00d3
_DMA1CFGH	=	0x00d3
Ftest_acc$DMA0CFGL$0$0 == 0x00d4
_DMA0CFGL	=	0x00d4
Ftest_acc$DMA0CFGH$0$0 == 0x00d5
_DMA0CFGH	=	0x00d5
Ftest_acc$DMAARM$0$0 == 0x00d6
_DMAARM	=	0x00d6
Ftest_acc$DMAREQ$0$0 == 0x00d7
_DMAREQ	=	0x00d7
Ftest_acc$TIMIF$0$0 == 0x00d8
_TIMIF	=	0x00d8
Ftest_acc$RFD$0$0 == 0x00d9
_RFD	=	0x00d9
Ftest_acc$T1CC0L$0$0 == 0x00da
_T1CC0L	=	0x00da
Ftest_acc$T1CC0H$0$0 == 0x00db
_T1CC0H	=	0x00db
Ftest_acc$T1CC1L$0$0 == 0x00dc
_T1CC1L	=	0x00dc
Ftest_acc$T1CC1H$0$0 == 0x00dd
_T1CC1H	=	0x00dd
Ftest_acc$T1CC2L$0$0 == 0x00de
_T1CC2L	=	0x00de
Ftest_acc$T1CC2H$0$0 == 0x00df
_T1CC2H	=	0x00df
Ftest_acc$ACC$0$0 == 0x00e0
_ACC	=	0x00e0
Ftest_acc$RFST$0$0 == 0x00e1
_RFST	=	0x00e1
Ftest_acc$T1CNTL$0$0 == 0x00e2
_T1CNTL	=	0x00e2
Ftest_acc$T1CNTH$0$0 == 0x00e3
_T1CNTH	=	0x00e3
Ftest_acc$T1CTL$0$0 == 0x00e4
_T1CTL	=	0x00e4
Ftest_acc$T1CCTL0$0$0 == 0x00e5
_T1CCTL0	=	0x00e5
Ftest_acc$T1CCTL1$0$0 == 0x00e6
_T1CCTL1	=	0x00e6
Ftest_acc$T1CCTL2$0$0 == 0x00e7
_T1CCTL2	=	0x00e7
Ftest_acc$IRCON2$0$0 == 0x00e8
_IRCON2	=	0x00e8
Ftest_acc$RFIF$0$0 == 0x00e9
_RFIF	=	0x00e9
Ftest_acc$T4CNT$0$0 == 0x00ea
_T4CNT	=	0x00ea
Ftest_acc$T4CTL$0$0 == 0x00eb
_T4CTL	=	0x00eb
Ftest_acc$T4CCTL0$0$0 == 0x00ec
_T4CCTL0	=	0x00ec
Ftest_acc$T4CC0$0$0 == 0x00ed
_T4CC0	=	0x00ed
Ftest_acc$T4CCTL1$0$0 == 0x00ee
_T4CCTL1	=	0x00ee
Ftest_acc$T4CC1$0$0 == 0x00ef
_T4CC1	=	0x00ef
Ftest_acc$B$0$0 == 0x00f0
_B	=	0x00f0
Ftest_acc$PERCFG$0$0 == 0x00f1
_PERCFG	=	0x00f1
Ftest_acc$ADCCFG$0$0 == 0x00f2
_ADCCFG	=	0x00f2
Ftest_acc$P0SEL$0$0 == 0x00f3
_P0SEL	=	0x00f3
Ftest_acc$P1SEL$0$0 == 0x00f4
_P1SEL	=	0x00f4
Ftest_acc$P2SEL$0$0 == 0x00f5
_P2SEL	=	0x00f5
Ftest_acc$P1INP$0$0 == 0x00f6
_P1INP	=	0x00f6
Ftest_acc$P2INP$0$0 == 0x00f7
_P2INP	=	0x00f7
Ftest_acc$U1CSR$0$0 == 0x00f8
_U1CSR	=	0x00f8
Ftest_acc$U1DBUF$0$0 == 0x00f9
_U1DBUF	=	0x00f9
Ftest_acc$U1BAUD$0$0 == 0x00fa
_U1BAUD	=	0x00fa
Ftest_acc$U1UCR$0$0 == 0x00fb
_U1UCR	=	0x00fb
Ftest_acc$U1GCR$0$0 == 0x00fc
_U1GCR	=	0x00fc
Ftest_acc$P0DIR$0$0 == 0x00fd
_P0DIR	=	0x00fd
Ftest_acc$P1DIR$0$0 == 0x00fe
_P1DIR	=	0x00fe
Ftest_acc$P2DIR$0$0 == 0x00ff
_P2DIR	=	0x00ff
Ftest_acc$DMA0CFG$0$0 == 0xffffd5d4
_DMA0CFG	=	0xffffd5d4
Ftest_acc$DMA1CFG$0$0 == 0xffffd3d2
_DMA1CFG	=	0xffffd3d2
Ftest_acc$FADDR$0$0 == 0xffffadac
_FADDR	=	0xffffadac
Ftest_acc$ADC$0$0 == 0xffffbbba
_ADC	=	0xffffbbba
;--------------------------------------------------------
; special function bits
;--------------------------------------------------------
	.area RSEG    (ABS,DATA)
	.org 0x0000
Ftest_acc$P0_0$0$0 == 0x0080
_P0_0	=	0x0080
Ftest_acc$P0_1$0$0 == 0x0081
_P0_1	=	0x0081
Ftest_acc$P0_2$0$0 == 0x0082
_P0_2	=	0x0082
Ftest_acc$P0_3$0$0 == 0x0083
_P0_3	=	0x0083
Ftest_acc$P0_4$0$0 == 0x0084
_P0_4	=	0x0084
Ftest_acc$P0_5$0$0 == 0x0085
_P0_5	=	0x0085
Ftest_acc$P0_6$0$0 == 0x0086
_P0_6	=	0x0086
Ftest_acc$P0_7$0$0 == 0x0087
_P0_7	=	0x0087
Ftest_acc$_TCON_0$0$0 == 0x0088
__TCON_0	=	0x0088
Ftest_acc$RFTXRXIF$0$0 == 0x0089
_RFTXRXIF	=	0x0089
Ftest_acc$_TCON_2$0$0 == 0x008a
__TCON_2	=	0x008a
Ftest_acc$URX0IF$0$0 == 0x008b
_URX0IF	=	0x008b
Ftest_acc$_TCON_4$0$0 == 0x008c
__TCON_4	=	0x008c
Ftest_acc$ADCIF$0$0 == 0x008d
_ADCIF	=	0x008d
Ftest_acc$_TCON_6$0$0 == 0x008e
__TCON_6	=	0x008e
Ftest_acc$URX1IF$0$0 == 0x008f
_URX1IF	=	0x008f
Ftest_acc$P1_0$0$0 == 0x0090
_P1_0	=	0x0090
Ftest_acc$P1_1$0$0 == 0x0091
_P1_1	=	0x0091
Ftest_acc$P1_2$0$0 == 0x0092
_P1_2	=	0x0092
Ftest_acc$P1_3$0$0 == 0x0093
_P1_3	=	0x0093
Ftest_acc$P1_4$0$0 == 0x0094
_P1_4	=	0x0094
Ftest_acc$P1_5$0$0 == 0x0095
_P1_5	=	0x0095
Ftest_acc$P1_6$0$0 == 0x0096
_P1_6	=	0x0096
Ftest_acc$P1_7$0$0 == 0x0097
_P1_7	=	0x0097
Ftest_acc$ENCIF_0$0$0 == 0x0098
_ENCIF_0	=	0x0098
Ftest_acc$ENCIF_1$0$0 == 0x0099
_ENCIF_1	=	0x0099
Ftest_acc$_SOCON2$0$0 == 0x009a
__SOCON2	=	0x009a
Ftest_acc$_SOCON3$0$0 == 0x009b
__SOCON3	=	0x009b
Ftest_acc$_SOCON4$0$0 == 0x009c
__SOCON4	=	0x009c
Ftest_acc$_SOCON5$0$0 == 0x009d
__SOCON5	=	0x009d
Ftest_acc$_SOCON6$0$0 == 0x009e
__SOCON6	=	0x009e
Ftest_acc$_SOCON7$0$0 == 0x009f
__SOCON7	=	0x009f
Ftest_acc$P2_0$0$0 == 0x00a0
_P2_0	=	0x00a0
Ftest_acc$P2_1$0$0 == 0x00a1
_P2_1	=	0x00a1
Ftest_acc$P2_2$0$0 == 0x00a2
_P2_2	=	0x00a2
Ftest_acc$P2_3$0$0 == 0x00a3
_P2_3	=	0x00a3
Ftest_acc$P2_4$0$0 == 0x00a4
_P2_4	=	0x00a4
Ftest_acc$P2_5$0$0 == 0x00a5
_P2_5	=	0x00a5
Ftest_acc$P2_6$0$0 == 0x00a6
_P2_6	=	0x00a6
Ftest_acc$P2_7$0$0 == 0x00a7
_P2_7	=	0x00a7
Ftest_acc$RFTXRXIE$0$0 == 0x00a8
_RFTXRXIE	=	0x00a8
Ftest_acc$ADCIE$0$0 == 0x00a9
_ADCIE	=	0x00a9
Ftest_acc$URX0IE$0$0 == 0x00aa
_URX0IE	=	0x00aa
Ftest_acc$URX1IE$0$0 == 0x00ab
_URX1IE	=	0x00ab
Ftest_acc$ENCIE$0$0 == 0x00ac
_ENCIE	=	0x00ac
Ftest_acc$STIE$0$0 == 0x00ad
_STIE	=	0x00ad
Ftest_acc$_IEN06$0$0 == 0x00ae
__IEN06	=	0x00ae
Ftest_acc$EA$0$0 == 0x00af
_EA	=	0x00af
Ftest_acc$DMAIE$0$0 == 0x00b8
_DMAIE	=	0x00b8
Ftest_acc$T1IE$0$0 == 0x00b9
_T1IE	=	0x00b9
Ftest_acc$T2IE$0$0 == 0x00ba
_T2IE	=	0x00ba
Ftest_acc$T3IE$0$0 == 0x00bb
_T3IE	=	0x00bb
Ftest_acc$T4IE$0$0 == 0x00bc
_T4IE	=	0x00bc
Ftest_acc$P0IE$0$0 == 0x00bd
_P0IE	=	0x00bd
Ftest_acc$_IEN16$0$0 == 0x00be
__IEN16	=	0x00be
Ftest_acc$_IEN17$0$0 == 0x00bf
__IEN17	=	0x00bf
Ftest_acc$DMAIF$0$0 == 0x00c0
_DMAIF	=	0x00c0
Ftest_acc$T1IF$0$0 == 0x00c1
_T1IF	=	0x00c1
Ftest_acc$T2IF$0$0 == 0x00c2
_T2IF	=	0x00c2
Ftest_acc$T3IF$0$0 == 0x00c3
_T3IF	=	0x00c3
Ftest_acc$T4IF$0$0 == 0x00c4
_T4IF	=	0x00c4
Ftest_acc$P0IF$0$0 == 0x00c5
_P0IF	=	0x00c5
Ftest_acc$_IRCON6$0$0 == 0x00c6
__IRCON6	=	0x00c6
Ftest_acc$STIF$0$0 == 0x00c7
_STIF	=	0x00c7
Ftest_acc$P$0$0 == 0x00d0
_P	=	0x00d0
Ftest_acc$F1$0$0 == 0x00d1
_F1	=	0x00d1
Ftest_acc$OV$0$0 == 0x00d2
_OV	=	0x00d2
Ftest_acc$RS0$0$0 == 0x00d3
_RS0	=	0x00d3
Ftest_acc$RS1$0$0 == 0x00d4
_RS1	=	0x00d4
Ftest_acc$F0$0$0 == 0x00d5
_F0	=	0x00d5
Ftest_acc$AC$0$0 == 0x00d6
_AC	=	0x00d6
Ftest_acc$CY$0$0 == 0x00d7
_CY	=	0x00d7
Ftest_acc$T3OVFIF$0$0 == 0x00d8
_T3OVFIF	=	0x00d8
Ftest_acc$T3CH0IF$0$0 == 0x00d9
_T3CH0IF	=	0x00d9
Ftest_acc$T3CH1IF$0$0 == 0x00da
_T3CH1IF	=	0x00da
Ftest_acc$T4OVFIF$0$0 == 0x00db
_T4OVFIF	=	0x00db
Ftest_acc$T4CH0IF$0$0 == 0x00dc
_T4CH0IF	=	0x00dc
Ftest_acc$T4CH1IF$0$0 == 0x00dd
_T4CH1IF	=	0x00dd
Ftest_acc$OVFIM$0$0 == 0x00de
_OVFIM	=	0x00de
Ftest_acc$_TIMIF7$0$0 == 0x00df
__TIMIF7	=	0x00df
Ftest_acc$ACC_0$0$0 == 0x00e0
_ACC_0	=	0x00e0
Ftest_acc$ACC_1$0$0 == 0x00e1
_ACC_1	=	0x00e1
Ftest_acc$ACC_2$0$0 == 0x00e2
_ACC_2	=	0x00e2
Ftest_acc$ACC_3$0$0 == 0x00e3
_ACC_3	=	0x00e3
Ftest_acc$ACC_4$0$0 == 0x00e4
_ACC_4	=	0x00e4
Ftest_acc$ACC_5$0$0 == 0x00e5
_ACC_5	=	0x00e5
Ftest_acc$ACC_6$0$0 == 0x00e6
_ACC_6	=	0x00e6
Ftest_acc$ACC_7$0$0 == 0x00e7
_ACC_7	=	0x00e7
Ftest_acc$P2IF$0$0 == 0x00e8
_P2IF	=	0x00e8
Ftest_acc$UTX0IF$0$0 == 0x00e9
_UTX0IF	=	0x00e9
Ftest_acc$UTX1IF$0$0 == 0x00ea
_UTX1IF	=	0x00ea
Ftest_acc$P1IF$0$0 == 0x00eb
_P1IF	=	0x00eb
Ftest_acc$WDTIF$0$0 == 0x00ec
_WDTIF	=	0x00ec
Ftest_acc$_IRCON25$0$0 == 0x00ed
__IRCON25	=	0x00ed
Ftest_acc$_IRCON26$0$0 == 0x00ee
__IRCON26	=	0x00ee
Ftest_acc$_IRCON27$0$0 == 0x00ef
__IRCON27	=	0x00ef
Ftest_acc$B_0$0$0 == 0x00f0
_B_0	=	0x00f0
Ftest_acc$B_1$0$0 == 0x00f1
_B_1	=	0x00f1
Ftest_acc$B_2$0$0 == 0x00f2
_B_2	=	0x00f2
Ftest_acc$B_3$0$0 == 0x00f3
_B_3	=	0x00f3
Ftest_acc$B_4$0$0 == 0x00f4
_B_4	=	0x00f4
Ftest_acc$B_5$0$0 == 0x00f5
_B_5	=	0x00f5
Ftest_acc$B_6$0$0 == 0x00f6
_B_6	=	0x00f6
Ftest_acc$B_7$0$0 == 0x00f7
_B_7	=	0x00f7
Ftest_acc$U1ACTIVE$0$0 == 0x00f8
_U1ACTIVE	=	0x00f8
Ftest_acc$U1TX_BYTE$0$0 == 0x00f9
_U1TX_BYTE	=	0x00f9
Ftest_acc$U1RX_BYTE$0$0 == 0x00fa
_U1RX_BYTE	=	0x00fa
Ftest_acc$U1ERR$0$0 == 0x00fb
_U1ERR	=	0x00fb
Ftest_acc$U1FE$0$0 == 0x00fc
_U1FE	=	0x00fc
Ftest_acc$U1SLAVE$0$0 == 0x00fd
_U1SLAVE	=	0x00fd
Ftest_acc$U1RE$0$0 == 0x00fe
_U1RE	=	0x00fe
Ftest_acc$U1MODE$0$0 == 0x00ff
_U1MODE	=	0x00ff
;--------------------------------------------------------
; overlayable register banks
;--------------------------------------------------------
	.area REG_BANK_0	(REL,OVR,DATA)
	.ds 8
	.area REG_BANK_2	(REL,OVR,DATA)
	.ds 8
	.area REG_BANK_3	(REL,OVR,DATA)
	.ds 8
;--------------------------------------------------------
; internal ram data
;--------------------------------------------------------
	.area DSEG    (DATA)
Ftest_acc$accSample$0$0==.
_accSample:
	.ds 2
;--------------------------------------------------------
; overlayable items in internal ram 
;--------------------------------------------------------
	.area OSEG    (OVR,DATA)
;--------------------------------------------------------
; Stack segment in internal ram 
;--------------------------------------------------------
	.area	SSEG	(DATA)
__start__stack:
	.ds	1

;--------------------------------------------------------
; indirectly addressable internal ram data
;--------------------------------------------------------
	.area ISEG    (DATA)
;--------------------------------------------------------
; absolute internal ram data
;--------------------------------------------------------
	.area IABS    (ABS,DATA)
	.area IABS    (ABS,DATA)
;--------------------------------------------------------
; bit data
;--------------------------------------------------------
	.area BSEG    (BIT)
Ftest_acc$initComplete$0$0==.
_initComplete:
	.ds 1
;--------------------------------------------------------
; paged external ram data
;--------------------------------------------------------
	.area PSEG    (PAG,XDATA)
Ftest_acc$counter$0$0==.
_counter:
	.ds 2
;--------------------------------------------------------
; external ram data
;--------------------------------------------------------
	.area XSEG    (XDATA)
Ftest_acc$SYNC1$0$0 == 0xdf00
_SYNC1	=	0xdf00
Ftest_acc$SYNC0$0$0 == 0xdf01
_SYNC0	=	0xdf01
Ftest_acc$PKTLEN$0$0 == 0xdf02
_PKTLEN	=	0xdf02
Ftest_acc$PKTCTRL1$0$0 == 0xdf03
_PKTCTRL1	=	0xdf03
Ftest_acc$PKTCTRL0$0$0 == 0xdf04
_PKTCTRL0	=	0xdf04
Ftest_acc$ADDR$0$0 == 0xdf05
_ADDR	=	0xdf05
Ftest_acc$CHANNR$0$0 == 0xdf06
_CHANNR	=	0xdf06
Ftest_acc$FSCTRL1$0$0 == 0xdf07
_FSCTRL1	=	0xdf07
Ftest_acc$FSCTRL0$0$0 == 0xdf08
_FSCTRL0	=	0xdf08
Ftest_acc$FREQ2$0$0 == 0xdf09
_FREQ2	=	0xdf09
Ftest_acc$FREQ1$0$0 == 0xdf0a
_FREQ1	=	0xdf0a
Ftest_acc$FREQ0$0$0 == 0xdf0b
_FREQ0	=	0xdf0b
Ftest_acc$MDMCFG4$0$0 == 0xdf0c
_MDMCFG4	=	0xdf0c
Ftest_acc$MDMCFG3$0$0 == 0xdf0d
_MDMCFG3	=	0xdf0d
Ftest_acc$MDMCFG2$0$0 == 0xdf0e
_MDMCFG2	=	0xdf0e
Ftest_acc$MDMCFG1$0$0 == 0xdf0f
_MDMCFG1	=	0xdf0f
Ftest_acc$MDMCFG0$0$0 == 0xdf10
_MDMCFG0	=	0xdf10
Ftest_acc$DEVIATN$0$0 == 0xdf11
_DEVIATN	=	0xdf11
Ftest_acc$MCSM2$0$0 == 0xdf12
_MCSM2	=	0xdf12
Ftest_acc$MCSM1$0$0 == 0xdf13
_MCSM1	=	0xdf13
Ftest_acc$MCSM0$0$0 == 0xdf14
_MCSM0	=	0xdf14
Ftest_acc$FOCCFG$0$0 == 0xdf15
_FOCCFG	=	0xdf15
Ftest_acc$BSCFG$0$0 == 0xdf16
_BSCFG	=	0xdf16
Ftest_acc$AGCCTRL2$0$0 == 0xdf17
_AGCCTRL2	=	0xdf17
Ftest_acc$AGCCTRL1$0$0 == 0xdf18
_AGCCTRL1	=	0xdf18
Ftest_acc$AGCCTRL0$0$0 == 0xdf19
_AGCCTRL0	=	0xdf19
Ftest_acc$FREND1$0$0 == 0xdf1a
_FREND1	=	0xdf1a
Ftest_acc$FREND0$0$0 == 0xdf1b
_FREND0	=	0xdf1b
Ftest_acc$FSCAL3$0$0 == 0xdf1c
_FSCAL3	=	0xdf1c
Ftest_acc$FSCAL2$0$0 == 0xdf1d
_FSCAL2	=	0xdf1d
Ftest_acc$FSCAL1$0$0 == 0xdf1e
_FSCAL1	=	0xdf1e
Ftest_acc$FSCAL0$0$0 == 0xdf1f
_FSCAL0	=	0xdf1f
Ftest_acc$TEST2$0$0 == 0xdf23
_TEST2	=	0xdf23
Ftest_acc$TEST1$0$0 == 0xdf24
_TEST1	=	0xdf24
Ftest_acc$TEST0$0$0 == 0xdf25
_TEST0	=	0xdf25
Ftest_acc$PA_TABLE0$0$0 == 0xdf2e
_PA_TABLE0	=	0xdf2e
Ftest_acc$IOCFG2$0$0 == 0xdf2f
_IOCFG2	=	0xdf2f
Ftest_acc$IOCFG1$0$0 == 0xdf30
_IOCFG1	=	0xdf30
Ftest_acc$IOCFG0$0$0 == 0xdf31
_IOCFG0	=	0xdf31
Ftest_acc$PARTNUM$0$0 == 0xdf36
_PARTNUM	=	0xdf36
Ftest_acc$VERSION$0$0 == 0xdf37
_VERSION	=	0xdf37
Ftest_acc$FREQEST$0$0 == 0xdf38
_FREQEST	=	0xdf38
Ftest_acc$LQI$0$0 == 0xdf39
_LQI	=	0xdf39
Ftest_acc$RSSI$0$0 == 0xdf3a
_RSSI	=	0xdf3a
Ftest_acc$MARCSTATE$0$0 == 0xdf3b
_MARCSTATE	=	0xdf3b
Ftest_acc$PKTSTATUS$0$0 == 0xdf3c
_PKTSTATUS	=	0xdf3c
Ftest_acc$VCO_VC_DAC$0$0 == 0xdf3d
_VCO_VC_DAC	=	0xdf3d
Ftest_acc$I2SCFG0$0$0 == 0xdf40
_I2SCFG0	=	0xdf40
Ftest_acc$I2SCFG1$0$0 == 0xdf41
_I2SCFG1	=	0xdf41
Ftest_acc$I2SDATL$0$0 == 0xdf42
_I2SDATL	=	0xdf42
Ftest_acc$I2SDATH$0$0 == 0xdf43
_I2SDATH	=	0xdf43
Ftest_acc$I2SWCNT$0$0 == 0xdf44
_I2SWCNT	=	0xdf44
Ftest_acc$I2SSTAT$0$0 == 0xdf45
_I2SSTAT	=	0xdf45
Ftest_acc$I2SCLKF0$0$0 == 0xdf46
_I2SCLKF0	=	0xdf46
Ftest_acc$I2SCLKF1$0$0 == 0xdf47
_I2SCLKF1	=	0xdf47
Ftest_acc$I2SCLKF2$0$0 == 0xdf48
_I2SCLKF2	=	0xdf48
Ftest_acc$USBADDR$0$0 == 0xde00
_USBADDR	=	0xde00
Ftest_acc$USBPOW$0$0 == 0xde01
_USBPOW	=	0xde01
Ftest_acc$USBIIF$0$0 == 0xde02
_USBIIF	=	0xde02
Ftest_acc$USBOIF$0$0 == 0xde04
_USBOIF	=	0xde04
Ftest_acc$USBCIF$0$0 == 0xde06
_USBCIF	=	0xde06
Ftest_acc$USBIIE$0$0 == 0xde07
_USBIIE	=	0xde07
Ftest_acc$USBOIE$0$0 == 0xde09
_USBOIE	=	0xde09
Ftest_acc$USBCIE$0$0 == 0xde0b
_USBCIE	=	0xde0b
Ftest_acc$USBFRML$0$0 == 0xde0c
_USBFRML	=	0xde0c
Ftest_acc$USBFRMH$0$0 == 0xde0d
_USBFRMH	=	0xde0d
Ftest_acc$USBINDEX$0$0 == 0xde0e
_USBINDEX	=	0xde0e
Ftest_acc$USBMAXI$0$0 == 0xde10
_USBMAXI	=	0xde10
Ftest_acc$USBCSIL$0$0 == 0xde11
_USBCSIL	=	0xde11
Ftest_acc$USBCSIH$0$0 == 0xde12
_USBCSIH	=	0xde12
Ftest_acc$USBMAXO$0$0 == 0xde13
_USBMAXO	=	0xde13
Ftest_acc$USBCSOL$0$0 == 0xde14
_USBCSOL	=	0xde14
Ftest_acc$USBCSOH$0$0 == 0xde15
_USBCSOH	=	0xde15
Ftest_acc$USBCNTL$0$0 == 0xde16
_USBCNTL	=	0xde16
Ftest_acc$USBCNTH$0$0 == 0xde17
_USBCNTH	=	0xde17
Ftest_acc$USBF0$0$0 == 0xde20
_USBF0	=	0xde20
Ftest_acc$USBF1$0$0 == 0xde22
_USBF1	=	0xde22
Ftest_acc$USBF2$0$0 == 0xde24
_USBF2	=	0xde24
Ftest_acc$USBF3$0$0 == 0xde26
_USBF3	=	0xde26
Ftest_acc$USBF4$0$0 == 0xde28
_USBF4	=	0xde28
Ftest_acc$USBF5$0$0 == 0xde2a
_USBF5	=	0xde2a
Ftest_acc$operationalTimeStamp$0$0==.
_operationalTimeStamp:
	.ds 4
LreportResults$buffer$2$2==.
_reportResults_buffer_2_2:
	.ds 32
;--------------------------------------------------------
; absolute external ram data
;--------------------------------------------------------
	.area XABS    (ABS,XDATA)
;--------------------------------------------------------
; external initialized ram data
;--------------------------------------------------------
	.area XISEG   (XDATA)
	.area HOME    (CODE)
	.area GSINIT0 (CODE)
	.area GSINIT1 (CODE)
	.area GSINIT2 (CODE)
	.area GSINIT3 (CODE)
	.area GSINIT4 (CODE)
	.area GSINIT5 (CODE)
	.area GSINIT  (CODE)
	.area GSFINAL (CODE)
	.area CSEG    (CODE)
;--------------------------------------------------------
; interrupt vector 
;--------------------------------------------------------
	.area HOME    (CODE)
__interrupt_vect:
	ljmp	__sdcc_gsinit_startup
	reti
	.ds	7
	ljmp	_ISR_ADC
	.ds	5
	reti
	.ds	7
	reti
	.ds	7
	reti
	.ds	7
	reti
	.ds	7
	reti
	.ds	7
	reti
	.ds	7
	reti
	.ds	7
	reti
	.ds	7
	reti
	.ds	7
	ljmp	_ISR_T3
	.ds	5
	ljmp	_ISR_T4
;--------------------------------------------------------
; global & static initialisations
;--------------------------------------------------------
	.area HOME    (CODE)
	.area GSINIT  (CODE)
	.area GSFINAL (CODE)
	.area GSINIT  (CODE)
	.globl __sdcc_gsinit_startup
	.globl __sdcc_program_startup
	.globl __start__stack
	.globl __mcs51_genXINIT
	.globl __mcs51_genXRAMCLEAR
	.globl __mcs51_genRAMCLEAR
	.area GSFINAL (CODE)
	ljmp	__sdcc_program_startup
;--------------------------------------------------------
; Home
;--------------------------------------------------------
	.area HOME    (CODE)
	.area HOME    (CODE)
__sdcc_program_startup:
	lcall	_main
;	return from main will lock up
	sjmp .
;--------------------------------------------------------
; code
;--------------------------------------------------------
	.area CSEG    (CODE)
;------------------------------------------------------------
;Allocation info for local variables in function 'ISR_T3'
;------------------------------------------------------------
;------------------------------------------------------------
	G$ISR_T3$0$0 ==.
	C$test_acc.c$294$0$0 ==.
;	src/test_acc/test_acc.c:294: ISR(T3, 3) {
;	-----------------------------------------
;	 function ISR_T3
;	-----------------------------------------
_ISR_T3:
	ar2 = 0x1a
	ar3 = 0x1b
	ar4 = 0x1c
	ar5 = 0x1d
	ar6 = 0x1e
	ar7 = 0x1f
	ar0 = 0x18
	ar1 = 0x19
	push	psw
	mov	psw,#0x18
	C$test_acc.c$298$1$1 ==.
;	src/test_acc/test_acc.c:298: T3IE = 0;
	clr	_T3IE
	C$test_acc.c$300$1$1 ==.
;	src/test_acc/test_acc.c:300: if(initComplete) {
	jnb	_initComplete,00102$
	C$test_acc.c$304$2$2 ==.
;	src/test_acc/test_acc.c:304: ADCCON3 = 0b10100001;
	mov	_ADCCON3,#0xA1
00102$:
	C$test_acc.c$308$1$1 ==.
;	src/test_acc/test_acc.c:308: T3IE = 1;
	setb	_T3IE
	pop	psw
	C$test_acc.c$309$1$1 ==.
	XG$ISR_T3$0$0 ==.
	reti
;	eliminated unneeded push/pop dpl
;	eliminated unneeded push/pop dph
;	eliminated unneeded push/pop b
;	eliminated unneeded push/pop acc
;------------------------------------------------------------
;Allocation info for local variables in function 'ISR_ADC'
;------------------------------------------------------------
;------------------------------------------------------------
	G$ISR_ADC$0$0 ==.
	C$test_acc.c$327$1$1 ==.
;	src/test_acc/test_acc.c:327: ISR(ADC, 2)
;	-----------------------------------------
;	 function ISR_ADC
;	-----------------------------------------
_ISR_ADC:
	ar2 = 0x12
	ar3 = 0x13
	ar4 = 0x14
	ar5 = 0x15
	ar6 = 0x16
	ar7 = 0x17
	ar0 = 0x10
	ar1 = 0x11
	push	acc
	push	psw
	mov	psw,#0x10
	C$test_acc.c$331$1$1 ==.
;	src/test_acc/test_acc.c:331: ADCIE = 0;
	clr	_ADCIE
	C$test_acc.c$334$1$1 ==.
;	src/test_acc/test_acc.c:334: if(initComplete)
	jnb	_initComplete,00102$
	C$test_acc.c$337$2$2 ==.
;	src/test_acc/test_acc.c:337: accSample = ADC >> 5;   // [3:0] of ADC are unreliable, we throw them away
	mov	_accSample,_ADC
	mov	a,(_ADC >> 8)
	swap	a
	rr	a
	xch	a,_accSample
	swap	a
	rr	a
	anl	a,#0x07
	xrl	a,_accSample
	xch	a,_accSample
	anl	a,#0x07
	xch	a,_accSample
	xrl	a,_accSample
	xch	a,_accSample
	mov	(_accSample + 1),a
00102$:
	C$test_acc.c$343$1$1 ==.
;	src/test_acc/test_acc.c:343: ADCIE = 1;
	setb	_ADCIE
	pop	psw
	pop	acc
	C$test_acc.c$344$1$1 ==.
	XG$ISR_ADC$0$0 ==.
	reti
;	eliminated unneeded push/pop dpl
;	eliminated unneeded push/pop dph
;	eliminated unneeded push/pop b
;------------------------------------------------------------
;Allocation info for local variables in function 'updateLeds'
;------------------------------------------------------------
;------------------------------------------------------------
	Ftest_acc$updateLeds$0$0 ==.
	C$test_acc.c$365$1$1 ==.
;	src/test_acc/test_acc.c:365: static void updateLeds()
;	-----------------------------------------
;	 function updateLeds
;	-----------------------------------------
_updateLeds:
	ar2 = 0x02
	ar3 = 0x03
	ar4 = 0x04
	ar5 = 0x05
	ar6 = 0x06
	ar7 = 0x07
	ar0 = 0x00
	ar1 = 0x01
	C$test_acc.c$367$1$1 ==.
;	src/test_acc/test_acc.c:367: usbShowStatusWithGreenLed(); // USB connected
	lcall	_usbShowStatusWithGreenLed
	C$test_acc.c$368$2$2 ==.
;	src/test_acc/test_acc.c:368: LED_YELLOW(vinPowerPresent());
	lcall	_vinPowerPresent
	jnc	00105$
	orl	_P2DIR,#0x04
	sjmp	00106$
00105$:
	anl	_P2DIR,#0xFB
00106$:
	C$test_acc.c$372$1$1 ==.
;	src/test_acc/test_acc.c:372: if(getMs() - operationalTimeStamp > 500)
	lcall	_getMs
	mov	r2,dpl
	mov	r3,dph
	mov	r4,b
	mov	r5,a
	mov	dptr,#_operationalTimeStamp
	movx	a,@dptr
	mov	r6,a
	inc	dptr
	movx	a,@dptr
	mov	r7,a
	inc	dptr
	movx	a,@dptr
	mov	r0,a
	inc	dptr
	movx	a,@dptr
	mov	r1,a
	mov	a,r2
	clr	c
	subb	a,r6
	mov	r2,a
	mov	a,r3
	subb	a,r7
	mov	r3,a
	mov	a,r4
	subb	a,r0
	mov	r4,a
	mov	a,r5
	subb	a,r1
	mov	r5,a
	clr	c
	mov	a,#0xF4
	subb	a,r2
	mov	a,#0x01
	subb	a,r3
	clr	a
	subb	a,r4
	clr	a
	subb	a,r5
	jnc	00103$
	C$test_acc.c$375$3$4 ==.
;	src/test_acc/test_acc.c:375: LED_RED_TOGGLE();
	xrl	_P2DIR,#0x02
	C$test_acc.c$378$2$3 ==.
;	src/test_acc/test_acc.c:378: operationalTimeStamp = getMs();
	lcall	_getMs
	mov	r2,dpl
	mov	r3,dph
	mov	r4,b
	mov	r5,a
	mov	dptr,#_operationalTimeStamp
	mov	a,r2
	movx	@dptr,a
	inc	dptr
	mov	a,r3
	movx	@dptr,a
	inc	dptr
	mov	a,r4
	movx	@dptr,a
	inc	dptr
	mov	a,r5
	movx	@dptr,a
00103$:
	C$test_acc.c$380$2$1 ==.
	XFtest_acc$updateLeds$0$0 ==.
	ret
;------------------------------------------------------------
;Allocation info for local variables in function 'reportResults'
;------------------------------------------------------------
;buffer                    Allocated with name '_reportResults_buffer_2_2'
;------------------------------------------------------------
	Ftest_acc$reportResults$0$0 ==.
	C$test_acc.c$383$2$1 ==.
;	src/test_acc/test_acc.c:383: static void reportResults()
;	-----------------------------------------
;	 function reportResults
;	-----------------------------------------
_reportResults:
	C$test_acc.c$385$1$1 ==.
;	src/test_acc/test_acc.c:385: if(usbComTxAvailable() > 32)
	lcall	_usbComTxAvailable
	mov	r2,dpl
	mov	a,#0x20
	cjne	a,ar2,00106$
00106$:
	jnc	00103$
	C$test_acc.c$391$2$2 ==.
;	src/test_acc/test_acc.c:391: bufferLength = sprintf(buffer, "%d \r\n", accSample);
	push	_accSample
	push	(_accSample + 1)
	mov	a,#__str_0
	push	acc
	mov	a,#(__str_0 >> 8)
	push	acc
	mov	a,#0x80
	push	acc
	mov	a,#_reportResults_buffer_2_2
	push	acc
	mov	a,#(_reportResults_buffer_2_2 >> 8)
	push	acc
	clr	a
	push	acc
	lcall	_sprintf
	mov	r2,dpl
	mov	a,sp
	add	a,#0xf8
	mov	sp,a
	mov	r0,#_usbComTxSend_PARM_2
	mov	a,r2
	movx	@r0,a
	C$test_acc.c$393$2$2 ==.
;	src/test_acc/test_acc.c:393: usbComTxSend(buffer, bufferLength);
	mov	dptr,#_reportResults_buffer_2_2
	C$test_acc.c$395$2$1 ==.
	XFtest_acc$reportResults$0$0 ==.
	ljmp	_usbComTxSend
00103$:
	ret
;------------------------------------------------------------
;Allocation info for local variables in function 'main'
;------------------------------------------------------------
;------------------------------------------------------------
	G$main$0$0 ==.
	C$test_acc.c$419$2$1 ==.
;	src/test_acc/test_acc.c:419: void main()
;	-----------------------------------------
;	 function main
;	-----------------------------------------
_main:
	C$test_acc.c$422$1$1 ==.
;	src/test_acc/test_acc.c:422: initComplete = FALSE;
	clr	_initComplete
	C$test_acc.c$425$1$1 ==.
;	src/test_acc/test_acc.c:425: systemInit();     // configures getMs() function, among others
	lcall	_systemInit
	C$test_acc.c$426$1$1 ==.
;	src/test_acc/test_acc.c:426: usbInit();        // Allows bootloader to be entered by USB command
	lcall	_usbInit
	C$test_acc.c$429$1$1 ==.
;	src/test_acc/test_acc.c:429: isrTimerInit();            // Initializes T3 timer for 10KHz sampling freq.
	lcall	_isrTimerInit
	C$test_acc.c$430$1$1 ==.
;	src/test_acc/test_acc.c:430: adcInit();                 // Initializes P0_0 as analog input
	lcall	_adcInit
	C$test_acc.c$432$1$1 ==.
;	src/test_acc/test_acc.c:432: accSample = 0;
	clr	a
	mov	_accSample,a
	mov	(_accSample + 1),a
	C$test_acc.c$433$1$1 ==.
;	src/test_acc/test_acc.c:433: counter = 0;
	mov	r0,#_counter
	clr	a
	movx	@r0,a
	inc	r0
	movx	@r0,a
	C$test_acc.c$435$1$1 ==.
;	src/test_acc/test_acc.c:435: operationalTimeStamp = getMs();
	lcall	_getMs
	mov	r2,dpl
	mov	r3,dph
	mov	r4,b
	mov	r5,a
	mov	dptr,#_operationalTimeStamp
	mov	a,r2
	movx	@dptr,a
	inc	dptr
	mov	a,r3
	movx	@dptr,a
	inc	dptr
	mov	a,r4
	movx	@dptr,a
	inc	dptr
	mov	a,r5
	movx	@dptr,a
	C$test_acc.c$438$1$1 ==.
;	src/test_acc/test_acc.c:438: initComplete = TRUE;
	setb	_initComplete
	C$test_acc.c$441$1$1 ==.
;	src/test_acc/test_acc.c:441: while(TRUE)
00106$:
	C$test_acc.c$443$2$2 ==.
;	src/test_acc/test_acc.c:443: ++counter;
	mov	r0,#_counter
	movx	a,@r0
	add	a,#0x01
	movx	@r0,a
	inc	r0
	movx	a,@r0
	addc	a,#0x00
	movx	@r0,a
	C$test_acc.c$444$2$2 ==.
;	src/test_acc/test_acc.c:444: if(counter == 1000) {
	mov	r0,#_counter
	movx	a,@r0
	cjne	a,#0xE8,00113$
	inc	r0
	movx	a,@r0
	cjne	a,#0x03,00113$
	sjmp	00114$
00113$:
	sjmp	00102$
00114$:
	C$test_acc.c$445$3$3 ==.
;	src/test_acc/test_acc.c:445: counter = 0;
	mov	r0,#_counter
	clr	a
	movx	@r0,a
	inc	r0
	movx	@r0,a
00102$:
	C$test_acc.c$448$2$2 ==.
;	src/test_acc/test_acc.c:448: if(!counter) {
	mov	r0,#_counter
	movx	a,@r0
	mov	b,a
	inc	r0
	movx	a,@r0
	orl	a,b
	jnz	00104$
	C$test_acc.c$449$3$4 ==.
;	src/test_acc/test_acc.c:449: reportResults();  // Display results from Accelerometer ADC conversion
	lcall	_reportResults
00104$:
	C$test_acc.c$451$2$2 ==.
;	src/test_acc/test_acc.c:451: updateLeds();     // blinks red LED every 50ms if we are receiving packets
	lcall	_updateLeds
	C$test_acc.c$452$2$2 ==.
;	src/test_acc/test_acc.c:452: boardService();   // so we can start bootloader by shorting P2_2 to 3V3
	lcall	_boardService
	C$test_acc.c$453$2$2 ==.
;	src/test_acc/test_acc.c:453: usbComService();  // so we can start bootloader from Wixel config GUI
	lcall	_usbComService
	C$test_acc.c$455$1$1 ==.
	XG$main$0$0 ==.
	sjmp	00106$
	.area CSEG    (CODE)
	.area CONST   (CODE)
Ftest_acc$_str_0$0$0 == .
__str_0:
	.ascii "%d "
	.db 0x0D
	.db 0x0A
	.db 0x00
	.area XINIT   (CODE)
	.area CABS    (ABS,CODE)
