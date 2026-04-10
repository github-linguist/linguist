/*
 * Simple PowerPC spinlock using lwarx/stwcx. reservation pair.
 * MIT License
 */

	.section .text
	.p2align 2

/*
 * void spin_lock(uint32_t *lock)
 *   r3 = pointer to lock word (0 = unlocked, 1 = locked)
 */
	.globl	spin_lock
	.type	spin_lock, @function
spin_lock:
1:
	lwarx	r5, 0, r3		/* load-reserve lock word */
	cmpwi	r5, 0
	bne-	2f			/* contended -- go spin */
	li	r5, 1
	stwcx.	r5, 0, r3		/* try to store 1 */
	bne-	1b			/* reservation lost, retry */
	isync
	blr

2:	/* spin on ordinary load to avoid flooding the bus */
	lwz	r5, 0(r3)
	cmpwi	r5, 0
	bne	2b
	b	1b

/*
 * void spin_unlock(uint32_t *lock)
 */
	.globl	spin_unlock
	.type	spin_unlock, @function
spin_unlock:
	lwsync				/* release barrier */
	li	r5, 0
	stw	r5, 0(r3)
	blr

/*
 * int spin_trylock(uint32_t *lock)
 *   Returns 1 on success, 0 on failure.
 */
	.globl	spin_trylock
	.type	spin_trylock, @function
spin_trylock:
	lwarx	r5, 0, r3
	cmpwi	r5, 0
	bne-	.trylock_fail
	li	r5, 1
	stwcx.	r5, 0, r3
	bne-	.trylock_fail
	isync
	li	r3, 1
	blr
.trylock_fail:
	li	r3, 0
	blr
