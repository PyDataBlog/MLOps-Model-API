
irq.o:     file format elf32-littlearm


Disassembly of section .text:

00000000 <init_entry_point>:
   0:	e59ff018 	ldr	pc, [pc, #24]	; 20 <reset_handler>
   4:	e59ff018 	ldr	pc, [pc, #24]	; 24 <basic_handler>
   8:	e59ff01c 	ldr	pc, [pc, #28]	; 2c <basic_handler_swi>
   c:	e59ff010 	ldr	pc, [pc, #16]	; 24 <basic_handler>
  10:	e59ff00c 	ldr	pc, [pc, #12]	; 24 <basic_handler>
  14:	e59ff008 	ldr	pc, [pc, #8]	; 24 <basic_handler>
  18:	e59ff008 	ldr	pc, [pc, #8]	; 28 <basic_handler_irq>
  1c:	e59ff000 	ldr	pc, [pc]	; 24 <basic_handler>

00000020 <reset_handler>:
  20:	00000038 	andeq	r0, r0, r8, lsr r0

00000024 <basic_handler>:
  24:	00000060 	andeq	r0, r0, r0, rrx

00000028 <basic_handler_irq>:
  28:	0000005c 	andeq	r0, r0, ip, asr r0

0000002c <basic_handler_swi>:
  2c:	00000070 	andeq	r0, r0, r0, ror r0

00000030 <return_addr>:
  30:	00000000 	andeq	r0, r0, r0

00000034 <saved_cpsr>:
  34:	00000000 	andeq	r0, r0, r0

00000038 <reset>:
  38:	e3a00902 	mov	r0, #32768	; 0x8000
  3c:	e3a01000 	mov	r1, #0
  40:	e8b003fc 	ldm	r0!, {r2, r3, r4, r5, r6, r7, r8, r9}
  44:	e8a103fc 	stmia	r1!, {r2, r3, r4, r5, r6, r7, r8, r9}
  48:	e8b003fc 	ldm	r0!, {r2, r3, r4, r5, r6, r7, r8, r9}
  4c:	e8a103fc 	stmia	r1!, {r2, r3, r4, r5, r6, r7, r8, r9}
  50:	eafffffe 	b	0 <init>
  54:	e12fff1e 	bx	lr

00000058 <hang>:
  58:	eafffffe 	b	58 <hang>

0000005c <arm_interrupt_handler_irq>:
  5c:	eafffffe 	b	0 <irq_handler>

00000060 <arm_interrupt_handler>:
  60:	e24ee004 	sub	lr, lr, #4
  64:	e92d5fff 	push	{r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, sl, fp, ip, lr}
  68:	ebffffed 	bl	24 <basic_handler>
  6c:	e8fd9fff 	ldm	sp!, {r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, sl, fp, ip, pc}^

00000070 <arm_interrupt_handler_swi>:
  70:	e3350001 	teq	r5, #1
  74:	0afffffe 	beq	0 <swi_svmode>
  78:	e92d5fff 	push	{r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, sl, fp, ip, lr}
  7c:	e3350000 	teq	r5, #0
  80:	0a000003 	beq	94 <swi_case_0>
  84:	e3350002 	teq	r5, #2
  88:	0a000003 	beq	9c <swi_case_2>
  8c:	e3350003 	teq	r5, #3
  90:	0a000003 	beq	a4 <swi_case_3>

00000094 <swi_case_0>:
  94:	ebfffffe 	bl	0 <swi_writer>
  98:	ea000003 	b	ac <arm_swi_end>

0000009c <swi_case_2>:
  9c:	ebfffffe 	bl	0 <swi_endTask>
  a0:	ea000001 	b	ac <arm_swi_end>

000000a4 <swi_case_3>:
  a4:	ebfffffe 	bl	0 <swi_addTask>
  a8:	eaffffff 	b	ac <arm_swi_end>

000000ac <arm_swi_end>:
  ac:	e8fd9fff 	ldm	sp!, {r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, sl, fp, ip, pc}^

000000b0 <armLoadContext>:
  b0:	e59f0044 	ldr	r0, [pc, #68]	; fc <armClearScreenloop+0x18>
  b4:	e5900000 	ldr	r0, [r0]
  b8:	e2400044 	sub	r0, r0, #68	; 0x44
  bc:	e5901000 	ldr	r1, [r0]
  c0:	e2800004 	add	r0, r0, #4
  c4:	e129f001 	msr	CPSR_fc, r1
  c8:	e890ffff 	ldm	r0, {r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, sl, fp, ip, sp, lr, pc}

000000cc <armRun>:
  cc:	e369f010 	msr	SPSR_fc, #16
  d0:	e92d0001 	stmfd	sp!, {r0}
  d4:	e8dd8000 	ldm	sp, {pc}^

000000d8 <armClearScreen>:
  d8:	e3a01000 	mov	r1, #0
  dc:	e5902024 	ldr	r2, [r0, #36]	; 0x24
  e0:	e5900020 	ldr	r0, [r0, #32]

000000e4 <armClearScreenloop>:
  e4:	e5801000 	str	r1, [r0]
  e8:	e2800004 	add	r0, r0, #4
  ec:	e2422004 	sub	r2, r2, #4
  f0:	e3520000 	cmp	r2, #0
  f4:	1afffffa 	bne	e4 <armClearScreenloop>
  f8:	e1a0f00e 	mov	pc, lr
  fc:	00000000 	andeq	r0, r0, r0

Disassembly of section .ARM.attributes:

00000000 <.ARM.attributes>:
   0:	00001541 	andeq	r1, r0, r1, asr #10
   4:	61656100 	cmnvs	r5, r0, lsl #2
   8:	01006962 	tsteq	r0, r2, ror #18
   c:	0000000b 	andeq	r0, r0, fp
  10:	01080206 	tsteq	r8, r6, lsl #4
  14:	Address 0x0000000000000014 is out of bounds.

