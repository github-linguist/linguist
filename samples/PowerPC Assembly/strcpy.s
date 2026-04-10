/*
 * Word-at-a-time strcpy for 32-bit PowerPC.
 * MIT License
 */

	.section .text
	.p2align 2

/*
 * char *strcpy(char *dst, const char *src)
 *   r3 = dst, r4 = src.  Returns dst in r3.
 *
 * Copies bytes until src is word-aligned, then checks four bytes
 * at a time for a null using the standard (word - 0x01010101) &
 * ~word & 0x80808080 trick.
 */
	.globl	strcpy
	.type	strcpy, @function
strcpy:
	mr	r9, r3

	/* byte loop until src is word-aligned */
	andi.	r5, r4, 3
	beq	.Laligned
.Lbyte_loop:
	lbz	r6, 0(r4)
	stb	r6, 0(r3)
	cmpwi	r6, 0
	beqlr
	addi	r3, r3, 1
	addi	r4, r4, 1
	andi.	r5, r4, 3
	bne	.Lbyte_loop

.Laligned:
	lis	r7, 0x0101
	ori	r7, r7, 0x0101		/* r7 = 0x01010101 */
	lis	r8, 0x8080
	ori	r8, r8, 0x8080		/* r8 = 0x80808080 */

.Lword_loop:
	lwz	r6, 0(r4)
	subf	r10, r7, r6
	andc	r10, r10, r6
	and.	r10, r10, r8
	bne	.Ltail
	stw	r6, 0(r3)
	addi	r3, r3, 4
	addi	r4, r4, 4
	b	.Lword_loop

.Ltail:
	lbz	r6, 0(r4)
	stb	r6, 0(r3)
	cmpwi	r6, 0
	beqlr
	lbz	r6, 1(r4)
	stb	r6, 1(r3)
	cmpwi	r6, 0
	beqlr
	lbz	r6, 2(r4)
	stb	r6, 2(r3)
	cmpwi	r6, 0
	beqlr
	li	r6, 0
	stb	r6, 3(r3)
	mr	r3, r9
	blr
