; Based on https://github.com/tattlemuss/lz4-m68k by Steven Tattersall under the MIT license
; depack lz4 buffer
; Based on the description at http://lz4.github.io/lz4/lz4_Block_format.html
; input a0 - start of compressed data
; input a1 - end of compressed data
; input a2 - start of output buffer
lz4_depack:
	moveq	#15,d4			; d4 = "15"
	moveq.l	#0,d0			; d0 = initial token fetch, high bits used to generate lengths
	moveq.l	#0,d2
	moveq.l	#0,d3			; Ensure clear high word (found by @zerkman)
.lz4_depack_loop:
	move.b	(a0)+,d0		; d0 = token, 0 in high bits
	move.l	d0,d1
	lsr.b	#4,d1			; d1 = length of literals
	beq.s	.no_literals
	bsr.s	.fetch_length
.literal_copy_loop:
	move.b	(a0)+,(a2)+
	subq.l	#1,d1
	bne.s	.literal_copy_loop

.no_literals:
	cmp.l	a1,a0			; Spec states that the last 5 bytes must be literals, so we do the check here.
	beq.s	.all_done

	move.b	(a0)+,d2		; d2 = offset low byte
	move.b	(a0)+,d3		; d3 = offset high byte
	lsl.w	#8,d3
	or.w	d2,d3			; d3 = offset 16 bits
	move.l	a2,a3
	sub.l	d3,a3			; a3 = bytes to copy

	move.l	d0,d1			; Now do match copy
	and.w	d4,d1			; d1 = length of token match
	bsr	.fetch_length
	addq.l	#4,d1			; Match length is +4
.match_copy_loop:
	move.b	(a3)+,(a2)+
	subq.l	#1,d1			; Note we can't use dbf since the match length could be > $ffff in theory?
	bne.s	.match_copy_loop
	bra.s	.lz4_depack_loop	; We go again.

.fetch_length:					; Fetch extra length bytes, reused for literals or match
	cmp.b	d4,d1			; If it's not 15, stop
	bne.s	.length_done
.more_literal_length:
	move.b	(a0)+,d2		; d2 = literal add value (255 == keep going) with 0 in high bits
	add.l	d2,d1			; d1 = updated literal length
	cmp.b	#255,d2
	beq.s	.more_literal_length
.length_done:
.all_done:
	rts
lz4_depack_size		equ	*-lz4_depack
