/*-
 * SPDX-License-Identifier: BSD-2-Clause-FreeBSD
 *
 * Copyright (c) 2018, Matthew Macy <mmacy@freebsd.org>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $FreeBSD$	
 */

/*
 * Assembly variants of various functions, for those that don't need generic C
 * implementations.  Currently this includes:
 *
 * - Direct-access versions of copyin/copyout methods.
 *   - These are used by Radix AIM pmap (ISA 3.0), and Book-E, to avoid
 *     unnecessary pmap_map_usr_ptr() calls.
 */

#include "assym.inc"
#include "opt_sched.h"

#include <sys/syscall.h>
#include <sys/errno.h>
	
#include <machine/param.h>
#include <machine/asm.h>
#include <machine/spr.h>
#include <machine/trap.h>
#include <machine/vmparam.h>

#ifdef _CALL_ELF
.abiversion _CALL_ELF
#endif

#ifdef __powerpc64__
#define	LOAD	ld
#define	STORE	std
#define	WORD	8
#define	CMPI	cmpdi
#define	CMPLI	cmpldi
/* log_2(8 * WORD) */
#define	LOOP_LOG	6
#define	LOG_WORD	3
#else
#define	LOAD	lwz
#define	STORE	stw
#define	WORD	4
#define	CMPI	cmpwi
#define	CMPLI	cmplwi
/* log_2(8 * WORD) */
#define	LOOP_LOG	5
#define	LOG_WORD	2
#endif

#ifdef AIM
#define ENTRY_DIRECT(x) ENTRY(x ## _direct)
#else
#define	ENTRY_DIRECT(x)	ENTRY(x)
#endif
	
#ifdef __powerpc64__
#define	PROLOGUE		;\
	mflr	%r0 		;\
	std	%r0, 16(%r1)	;\

#define	EPILOGUE		;\
	ld	%r0, 16(%r1)	;\
	mtlr	%r0		;\
	blr			;\
	nop

#define	VALIDATE_TRUNCATE_ADDR_COPY	VALIDATE_ADDR_COPY
#define	VALIDATE_ADDR_COPY(raddr, len)	\
	srdi  %r0, raddr, 52		;\
	cmpwi %r0, 1			;\
	bge-	copy_fault		;\
	nop

#define	VALIDATE_ADDR_FUSU(raddr)	;\
	srdi  %r0, raddr, 52		;\
	cmpwi %r0, 1			;\
	bge-	fusufault		;\
	nop

#else
#define	PROLOGUE		;\
	mflr	%r0 		;\
	stw	%r0, 4(%r1)	;\

#define	EPILOGUE		;\
	lwz	%r0, 4(%r1)	;\
	mtlr	%r0		;\
	blr			;\
	nop

/* %r0 is temporary */
/*
 * Validate address and length are valid.
 * For VALIDATE_ADDR_COPY() have to account for wraparound.
 */
#define	VALIDATE_ADDR_COPY(raddr, len)		\
	lis	%r0, VM_MAXUSER_ADDRESS@h	;\
	ori	%r0, %r0, VM_MAXUSER_ADDRESS@l	;\
	cmplw	%r0, raddr			;\
	blt-	copy_fault			;\
	add	%r0, raddr, len			;\
	cmplw	7, %r0, raddr			;\
	blt-	7, copy_fault			;\
	mtcrf	0x80, %r0			;\
	bt-	0, copy_fault			;\
	nop

#define	VALIDATE_TRUNCATE_ADDR_COPY(raddr, len)		\
	lis	%r0, VM_MAXUSER_ADDRESS@h	;\
	ori	%r0, %r0, VM_MAXUSER_ADDRESS@l	;\
	cmplw	%r0, raddr			;\
	blt-	copy_fault			;\
	sub	%r0, %r0, raddr			;\
	cmplw	len, %r0			;\
	isel	len, len, %r0, 0		;\

#define	VALIDATE_ADDR_FUSU(raddr)		\
	lis	%r0, VM_MAXUSER_ADDRESS@h	;\
	ori	%r0, %r0, VM_MAXUSER_ADDRESS@l	;\
	cmplw	%r0, raddr			;\
	ble-	fusufault

#endif

#define PCPU(reg) mfsprg  reg, 0

#define	SET_COPYFAULT(raddr, rpcb, len)	\
	VALIDATE_ADDR_COPY(raddr, len)	;\
	PCPU(%r9)			;\
	li	%r0, COPYFAULT		;\
	LOAD	rpcb, PC_CURPCB(%r9)	;\
	STORE	%r0, PCB_ONFAULT(rpcb)	;\

#define	SET_COPYFAULT_TRUNCATE(raddr, rpcb, len)\
	VALIDATE_TRUNCATE_ADDR_COPY(raddr, len)	;\
	PCPU(%r9)				;\
	li	%r0, COPYFAULT			;\
	LOAD	rpcb, PC_CURPCB(%r9)		;\
	STORE	%r0, PCB_ONFAULT(rpcb)

#define	SET_FUSUFAULT(raddr, rpcb)	\
	VALIDATE_ADDR_FUSU(raddr)	;\
	PCPU(%r9)			;\
	li	%r0, FUSUFAULT		;\
	LOAD	rpcb, PC_CURPCB(%r9)	;\
	STORE	%r0, PCB_ONFAULT(rpcb)

#define	CLEAR_FAULT_NO_CLOBBER(rpcb)	\
	PCPU(%r9)			;\
	LOAD	rpcb, PC_CURPCB(%r9)	;\
	li	%r0, 0			;\
	STORE	%r0, PCB_ONFAULT(rpcb)

#define	CLEAR_FAULT(rpcb)		\
	CLEAR_FAULT_NO_CLOBBER(rpcb)	;\
	li	%r3, 0

/*
 *  bcopy(src, dst, len)
 *        %r3  %r4  %r5
 * 
 *  %r7 is the pcb pointer
 * 
 *  %r0 and %r8-%r10 are volatile
 *  %r11 and %r12 are generally volatile, used in linking and exception
 *  handling.  Can be clobbered here.
 *
 * Does not allocate or use stack space, but clobbers all volatile registers.
 */

#define	rs	%r3
#define	rd	%r4
#define	rl	%r5

#define	t1	%r6
#define	t2	%r7
#define	t3	%r8
#define	t4	%r9
#define	t5	%r10
#define	t6	%r11
#define	t7	%r12
#define	t8	%r0

#define Thresh	WORD * 8
#define	W4	3
#define	W2	2
#define	W1	1
#define	WORDS(n)	(32 - LOG_WORD - W##n)
.text
ENTRY(bcopy_generic)
	CMPLI	0, %r5, 0
	beq	.Lend
	dcbtst	0, rd
	dcbt	0, rs
	CMPLI	rl, Thresh
	blt	.Lsmall
	b	.Llarge
/* memcpy */
/* ... */
.Lsmall: 				/* < 8 words remaining */
	mtcrf	0x3, rl
.Lsmall_start:
	bf	WORDS(4), 0f
	LOAD	t1, 0(rs)
	LOAD	t2, WORD*1(rs)
	LOAD	t3, WORD*2(rs)
	LOAD	t4, WORD*3(rs)
	addi	rs, rs, WORD*4
	STORE	t1, 0(rd)
	STORE	t2, WORD*1(rd)
	STORE	t3, WORD*2(rd)
	STORE	t4, WORD*3(rd)
	addi	rd, rd, WORD*4
0:					/* < 4 words remaining */
	bf	WORDS(2), 1f
	LOAD	t1, 0(rs)
	LOAD	t2, WORD*1(rs)
	addi	rs, rs, WORD*2
	STORE	t1, 0(rd)
	STORE	t2, WORD*1(rd)
	addi	rd, rd, WORD*2
1:					/* < 2 words remaining */
	bf	WORDS(1), 2f
	LOAD	t1, 0(rs)
	addi	rs, rs, WORD
	STORE	t1, 0(rd)
	addi	rd, rd, WORD
2:					/* < 1 word remaining */
#ifdef __powerpc64__
	bf	29, 3f
	lwz	t1, 0(rs)
	addi	rs, rs, 4
	stw	t1, 0(rd)
	addi	rd, rd, 4
3:					/* < 4 bytes remaining */
#endif
	bf	30, 4f
	lhz	t1, 0(rs)
	addi	rs, rs, 2
	sth	t1, 0(rd)
	addi	rd, rd, 2
4:					/* < 2 bytes remaining */
	bf	31, .Lout
	lbz	t1, 0(rs)
	addi	rs, rs, 1
	stb	t1, 0(rd)
	addi	rd, rd, 1
	b	.Lout

	.align 4
.Llarge:
	neg	t3, rd
	andi.	t6, t3, WORD-1		/* Align rd to word size */
	mtctr	t6
	sub	rl, rl, t6
	beq+	.Llargealigned
1:
	lbz	t1, 0(rs)
	addi	rs, rs, 1
	stb	t1, 0(rd)
	addi	rd, rd, 1
	bdnz	1b

.Llargealigned:
	srwi.	t2, rl, LOOP_LOG  /* length >> log_2(loop_size) => 8W iterations */
	mtcrf	0x3, rl
	beq	.Lsmall_start
	mtctr	t2
	b	1f

	.align 5
1:
	LOAD	t1, 0(rs)
	LOAD	t2, WORD(rs)
	LOAD	t3, WORD*2(rs)
	LOAD	t4, WORD*3(rs)
	LOAD	t5, WORD*4(rs)
	LOAD	t6, WORD*5(rs)
	LOAD	t7, WORD*6(rs)
	LOAD	t8, WORD*7(rs)
	addi	rs, rs, WORD*8
	STORE	t1, 0(rd)
	STORE	t2, WORD*1(rd)
	STORE	t3, WORD*2(rd)
	STORE	t4, WORD*3(rd)
	STORE	t5, WORD*4(rd)
	STORE	t6, WORD*5(rd)
	STORE	t7, WORD*6(rd)
	STORE	t8, WORD*7(rd)
	addi	rd, rd, WORD*8
	bdnz	1b

	b	.Lsmall_start
.Lout:
/* done */	
.Lend:	
	blr

/*
 * copyout(from_kernel, to_user, len)
 *         %r3,        %r4,    %r5
 */
ENTRY_DIRECT(copyout)
	PROLOGUE
	SET_COPYFAULT(%r4, %r7, %r5)
	bl bcopy_generic 
	nop
	CLEAR_FAULT(%r7)
	EPILOGUE

/*
 * copyin(from_user, to_kernel, len)
 *        %r3,        %r4,    %r5
 */
ENTRY_DIRECT(copyin)
	PROLOGUE
	SET_COPYFAULT(%r3, %r7, %r5)
	bl bcopy_generic
	nop
	CLEAR_FAULT(%r7)
	EPILOGUE
/*
 * copyinstr(const void *udaddr, void *kaddr, size_t len, size_t *done)
 *			%r3          %r4         %r5        %r6 
 */
	
ENTRY_DIRECT(copyinstr)
	PROLOGUE
	SET_COPYFAULT_TRUNCATE(%r3, %r7, %r5)
	addi	%r9, %r5, 1
	mtctr	%r9
	mr	%r8, %r3
	addi	%r8, %r8, -1
	addi	%r4, %r4, -1
	li	%r3, ENAMETOOLONG
0:
	bdz-	2f
	lbzu	%r0, 1(%r8)
	stbu	%r0, 1(%r4)

	// NULL byte reached ?
	CMPI	%r0, 0
	beq-	1f
	b	0b
1:
	li	%r3, 0
2:
	/* skip storing length if done is NULL */
	CMPI	%r6, 0
	beq-	3f
	mfctr	%r0
	sub	%r0, %r9, %r0
	STORE	%r0, 0(%r6)
3:
	CLEAR_FAULT_NO_CLOBBER(%r7)
	EPILOGUE

ENTRY_DIRECT(subyte)
	PROLOGUE
	SET_FUSUFAULT(%r3, %r7)
	stb  %r4, 0(%r3)
	CLEAR_FAULT(%r7)
	EPILOGUE

#ifndef __powerpc64__
ENTRY_DIRECT(suword)
	PROLOGUE
	SET_FUSUFAULT(%r3, %r7)
	stw  %r4, 0(%r3)
	CLEAR_FAULT(%r7)
	EPILOGUE
#endif	

ENTRY_DIRECT(suword32)
	PROLOGUE
	SET_FUSUFAULT(%r3, %r7)
	stw  %r4, 0(%r3)
	CLEAR_FAULT(%r7)
	EPILOGUE

#ifdef __powerpc64__	
ENTRY_DIRECT(suword64)
	PROLOGUE
	SET_FUSUFAULT(%r3, %r7)
	std  %r4, 0(%r3) 
	CLEAR_FAULT(%r7)
	EPILOGUE
ENTRY_DIRECT(suword)
	PROLOGUE
	SET_FUSUFAULT(%r3, %r7)
	std  %r4, 0(%r3) 
	CLEAR_FAULT(%r7)
	EPILOGUE
#endif	
	
ENTRY_DIRECT(fubyte)
	PROLOGUE
	SET_FUSUFAULT(%r3, %r7)
	lbz %r3, 0(%r3)
	CLEAR_FAULT_NO_CLOBBER(%r7)
	EPILOGUE

ENTRY_DIRECT(fuword16)
	PROLOGUE
	SET_FUSUFAULT(%r3, %r7)
	lhz %r3, 0(%r3)
	CLEAR_FAULT_NO_CLOBBER(%r7)
	EPILOGUE

#ifndef __powerpc64__
ENTRY_DIRECT(fueword)	
	PROLOGUE
	SET_FUSUFAULT(%r3, %r7)
	lwz  %r0, 0(%r3)
	stw  %r0,  0(%r4)
	CLEAR_FAULT(%r7)
	EPILOGUE
#endif	
ENTRY_DIRECT(fueword32)
	PROLOGUE
	SET_FUSUFAULT(%r3, %r7)
	lwz  %r0, 0(%r3)
	stw  %r0,  0(%r4)
	CLEAR_FAULT(%r7)
	EPILOGUE

#ifdef __powerpc64__
ENTRY_DIRECT(fueword)	
	PROLOGUE
	SET_FUSUFAULT(%r3, %r7)
	ld  %r0, 0(%r3)
	std %r0, 0(%r4)
	CLEAR_FAULT(%r7)
	EPILOGUE

ENTRY_DIRECT(fueword64)
	PROLOGUE
	SET_FUSUFAULT(%r3, %r7)
	ld  %r0, 0(%r3)
	std %r0, 0(%r4)
	CLEAR_FAULT(%r7)
	EPILOGUE
#endif

/*
 * casueword(volatile u_long *base, u_long old, u_long *oldp, u_long new)
 *			      %r3          %r4           %r5         %r6 
 */

#define	CASUEWORD32(raddr, rpcb)					;\
	PROLOGUE							;\
	SET_FUSUFAULT(raddr, rpcb)					;\
	li	%r8, 0							;\
1:									;\
	lwarx	%r0, 0, %r3						;\
	cmplw	%r4, %r0						;\
	bne	2f							;\
	stwcx.	%r6, 0, %r3						;\
	bne-	3f							;\
	b	4f							;\
2:									;\
	stwcx.	%r0, 0, %r3       	/* clear reservation (74xx) */	;\
3:									;\
	li	%r8, 1							;\
4:									;\
	stw	%r0, 0(%r5)						;\
	CLEAR_FAULT_NO_CLOBBER(rpcb)					;\
	mr	%r3, %r8						;\
	EPILOGUE	
	
ENTRY_DIRECT(casueword32)
	CASUEWORD32(%r3, %r7)

#ifdef __powerpc64__
#define	CASUEWORD64(raddr, rpcb)					;\
	PROLOGUE							;\
	SET_FUSUFAULT(raddr, rpcb)					;\
	li	%r8, 0							;\
1:									;\
	ldarx	%r0, 0, %r3						;\
	cmpld	%r4, %r0						;\
	bne	2f							;\
	stdcx.	%r6, 0, %r3						;\
	bne-	3f							;\
	b	4f							;\
2:									;\
	stdcx.	%r0, 0, %r3       	/* clear reservation (74xx) */	;\
3:									;\
	li	%r8, 1							;\
4:									;\
	std	%r0, 0(%r5)						;\
	CLEAR_FAULT_NO_CLOBBER(rpcb)					;\
	mr	%r3, %r8						;\
	EPILOGUE

ENTRY_DIRECT(casueword)
	CASUEWORD64(%r3, %r7)

ENTRY_DIRECT(casueword64)
	CASUEWORD64(%r3, %r7)
#else
ENTRY_DIRECT(casueword)
	CASUEWORD32(%r3, %r7)
#endif
	
_NAKED_ENTRY(fusufault)
	CLEAR_FAULT_NO_CLOBBER(%r7)
	li %r3, -1
	EPILOGUE

_NAKED_ENTRY(copy_fault)
	CLEAR_FAULT_NO_CLOBBER(%r7)
	li %r3, EFAULT
	EPILOGUE
