/*
 * Data Storage Interrupt (DSI) handler for bare-metal PowerPC.
 * Saves trap frame, calls into C, restores and returns via rfi.
 * MIT License
 */

#define SPRG_SCRATCH0	272
#define SPRG_SCRATCH1	273

	.section .text
	.p2align 2

/*
 * DSI vector -- entered on data access fault (vector 0x300).
 * SRR0 = faulting PC, SRR1 = saved MSR.
 * DSISR (SPR 18) = cause, DAR (SPR 19) = faulting address.
 */
	.globl	dsi_vector
	.type	dsi_vector, @function
dsi_vector:
	/* stash r0/r1 in scratch SPRGs to free up working regs */
	mtspr	SPRG_SCRATCH0, r0
	mtspr	SPRG_SCRATCH1, r1

	mfsprg0	r1			/* kernel stack pointer */
	stwu	r1, -160(r1)		/* allocate trap frame */

	/* save GPRs */
	mfspr	r0, SPRG_SCRATCH0
	stw	r0, 12(r1)
	mfspr	r0, SPRG_SCRATCH1
	stw	r0, 16(r1)
	stw	r2, 20(r1)
	stw	r3, 24(r1)
	stw	r4, 28(r1)
	stw	r5, 32(r1)

	/* save exception SPRs */
	mfsrr0	r3
	stw	r3, 132(r1)
	mfsrr1	r3
	stw	r3, 136(r1)
	mfspr	r3, 18			/* DSISR */
	stw	r3, 140(r1)
	mfspr	r3, 19			/* DAR */
	stw	r3, 144(r1)
	mflr	r3
	stw	r3, 148(r1)
	mfcr	r3
	stw	r3, 152(r1)

	mr	r3, r1
	bl	dsi_handler		/* dsi_handler(struct trapframe *) */

	/* restore and return */
	lwz	r3, 152(r1)
	mtcr	r3
	lwz	r3, 148(r1)
	mtlr	r3
	lwz	r3, 132(r1)
	mtsrr0	r3
	lwz	r3, 136(r1)
	mtsrr1	r3

	lwz	r5, 32(r1)
	lwz	r4, 28(r1)
	lwz	r3, 24(r1)
	lwz	r2, 20(r1)
	lwz	r0, 12(r1)
	lwz	r1, 16(r1)
	rfi
