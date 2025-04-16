; See http://wiki.amigaos.net/wiki/ILBM_IFF_Interleaved_Bitmap

; ******** IFF_GetSize ********
; Retrieves Width, Height and the number of bitplans of a IFF Picture
; INPUT:	a0 = IFF buffer
; OUTPUT:	d0 = width or 0 if error
;			d1 = height, or 0 if error
;			d2 = bitplans, or 0 if error
IFF_GetSize:
	movem.l d3-d7/a0-a6,-(a7)
	bsr iff_getBMHDInfo
	movem.l (a7)+,d3-d7/a0-a6
	rts


; ******** IFF_GetPicture ********
; Uncompress a IFF ILBM picture to a pre-allocated buffer
; INPUT:	a0 = IFF ILBM source
;			a1 = destination buffer
; OUTPUT:	d0 = error code (0 = success, 1 = invalid format)
IFF_GetPicture:
	movem.l d1-d7/a0-a6,-(a7)
	bsr iff_uncompress
	movem.l (a7)+,d1-d7/a0-a6
	rts
	
; ******** IFF_GetPalette ********
; Finds the Palette in R,G,B format in the IFF buffer
; INPUT:	a0 = IFF ILBM source
; OUTPUT:	d0 = Palette pointer, or 0 if not found
IFF_GetPalette:
	movem.l d1-d7/a0-a6,-(a7)
	bsr iff_get_palette
	movem.l (a7)+,d1-d7/a0-a6
	rts

; *************************************************************************************************
iff_get_palette:
	moveq #0, d0
	cmp.l #"FORM",(a0)+
	bne .palette_end
	move.l (a0)+,d7		; remaining size
	add.l a0,d7			; d0 = end of file
	cmp.l #"ILBM",(a0)+
	bne .palette_end
.palette_next_chunk:
	cmp.l #"CMAP",(a0)+
	beq .palette_chunk
	add.l (a0)+,a0	; skip this chunk (BODY, CMAP...)
	cmp.l d7, a0
	blo .palette_next_chunk
	bra .palette_end	; no BMHD chunk!
.palette_chunk:
	addq #4, a0	; chunck size
	move.l a0, d0
.palette_end
	rts

; *************************************************************************************************
iff_getBMHDInfo:
	moveq #0, d0
	moveq #0, d1
	moveq #0, d2
	cmp.l #"FORM",(a0)+
	bne .bmhd_end
	move.l (a0)+,d7		; remaining size
	add.l a0,d7			; d0 = end of file
	cmp.l #"ILBM",(a0)+
	bne .bmhd_end
.bmhd_next_chunk:
	cmp.l #"BMHD",(a0)+
	beq .bmhd_chunk
	add.l (a0)+,a0	; skip this chunk (BODY, CMAP...)
	cmp.l d7, a0
	blo .bmhd_next_chunk
	bra .bmhd_end	; no BMHD chunk!
.bmhd_chunk:
	addq #4, a0	; chunck size
	move.w (a0),d0
	move.w 2(a0),d1
	move.b 8(a0),d2
.bmhd_end
	rts
	
; *************************************************************************************************
iff_uncompress:
	cmp.l #"FORM",(a0)+
	bne .iff_failure
	move.l (a0)+,d0		; remaining size
	add.l a0,d0		; d0 = end of file
	cmp.l #"ILBM",(a0)+
	bne .iff_failure
.iff_next_chunk:
	cmp.l #"BODY",(a0)+
	beq .iff_body
	add.l (a0)+,a0	; skip this chunk (BMHD, CMAP...)
	cmp.l d0, a0
	blo .iff_next_chunk
	bra .iff_failure	; no body chunk!
.iff_body:
	move.l (a0)+,d0	; size of body chunk
	add.l a0,d0	; end of body chunk
.iff_next_byte:
	moveq.l #0,d1
	move.b (a0)+,d1	; read next compressed byte
	cmp.b #128,d1
	bhs .iff_repeat
.iff_copy:
	move.b (a0)+,(a1)+
	dbf d1, .iff_copy
	cmp.l d0,a0
	blo .iff_next_byte
	bra .iff_success
.iff_repeat:
	neg.b d1
	move.b (a0)+,d2
.iff_repeat_loop:
	move.b d2,(a1)+
	dbf d1, .iff_repeat_loop
	cmp.l d0,a0
	blo .iff_next_byte
.iff_success:
	moveq.l #0, d0
	bra .iff_exit	
.iff_failure:
	moveq.l #1, d0
.iff_exit:
	rts
; ******** end iff_uncompress ********

