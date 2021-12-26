
// https://github.com/emmanuel-marty/apultra
// ported to Kick asm

.segment ZP
	apl_bitbuf:	.byte 0 
	apl_offset:	.word 0 
	apl_winptr:	.word 0 
	apl_srcptr:	.word 0 
	apl_dstptr:	.word 0 

.label apl_length = apl_winptr

.macro APL_INC_PAGE() {
	inc	<apl_srcptr + 1
}

.macro APL_GET_SRC() {
	lda     (apl_srcptr),y
	inc     <apl_srcptr + 0
	bne     skip
	APL_INC_PAGE()
skip:
}

.macro APUltraUnpack(source,dest) {
	lda #<source
	ldy #>source 
	sta apl_srcptr
	sty apl_srcptr+1 
	lda #<dest 
	ldy #>dest
	sta apl_dstptr
	sty apl_dstptr+1
	jsr APUltraDecompressRoutine
}

.segment CODE 
APUltraDecompressRoutine:
{
	ldy     #0                      // Initialize source index_

	lda     #$80                    // Initialize an empty
	sta     <apl_bitbuf             // bit-buffer_

	//
	// 0 bbbbbbbb - One byte from compressed data, i_e_ a "literal"_
	//

_literal:	APL_GET_SRC()
_write_byte:
	ldx     #0                      // LWM=0_

	sta     (apl_dstptr),y          // Write the byte directly to
	inc     <apl_dstptr + 0         // the output_
	bne     _next_tag
	inc     <apl_dstptr + 1

_next_tag:
	asl     <apl_bitbuf             // 0 bbbbbbbb
	bne     _skip0
	jsr     _load_bit
_skip0:
	bcc     _literal

_skip1:
	asl     <apl_bitbuf             // 1 0 <offset> <length>
	bne     _skip2
	jsr     _load_bit
_skip2:
  bcc     _copy_large

	asl     <apl_bitbuf             // 1 1 0 dddddddn
	bne     _skip3
	jsr     _load_bit
_skip3:
	bcc     _copy_normal

	// 1 1 1 dddd - Copy 1 byte within 15 bytes (or zero)_

_copy_short:
	lda     #$10
_nibble_loop:
	asl     <apl_bitbuf
	bne     _skip4
	pha
	jsr     _load_bit
	pla
_skip4:
	rol
	bcc     _nibble_loop
	beq     _write_byte             // Offset=0 means write zero_

	eor     #$FF                    // Read the byte directly from
	tay                             // the destination window_
	iny
	dec     <apl_dstptr + 1
	lda     (apl_dstptr),y
	inc     <apl_dstptr + 1
	ldy     #0
	beq     _write_byte

//
// 1 1 0 dddddddn - Copy 2 or 3 within 128 bytes_
//

_copy_normal:
	APL_GET_SRC()                    // 1 1 0 dddddddn
	lsr
	beq     _finished               // Offset 0 == EOF_

	sta     <apl_offset + 0         // Preserve offset_
	sty     <apl_offset + 1
	tya                             // Y == 0_
	tax                             // Bits 8__15 of length_
	adc     #2                      // Bits 0___7 of length_
	bne     _do_match               // NZ from previous ADC_

	//
	// Subroutines for byte & bit handling_
	//

_get_gamma:
	lda     #1                      // Get a gamma-coded value_
_gamma_loop:
	asl     <apl_bitbuf
	bne     _skip5
	pha
	jsr     _load_bit
	pla
_skip5:
	rol
	rol     <apl_length + 1
	asl     <apl_bitbuf
	bne     _skip6
	pha
	jsr     _load_bit
	pla
_skip6:
	bcs     _gamma_loop

_finished:
	rts                             // All decompressed!

//
// 1 0 <offset> <length> - gamma-coded LZSS pair_
//

_copy_large:
	jsr     _get_gamma              // Bits 8__15 of offset (min 2)_
	sty     <apl_length + 1         // Clear hi-byte of length_

	cpx     #1                      // CC if LWM==0, CS if LWM==1_
	sbc     #2                      // -3 if LWM==0, -2 if LWM==1_
	bcs     _normal_pair            // CC if LWM==0 && offset==2_

	jsr     _get_gamma              // Get length (A=lo-byte & CC)_
	ldx     <apl_length + 1
	bcc     _do_match               // Use previous Offset_

_normal_pair:
	sta     <apl_offset + 1         // Save bits 8__15 of offset_

	APL_GET_SRC()
	sta     <apl_offset + 0         // Save bits 0___7 of offset_

	jsr     _get_gamma              // Get length (A=lo-byte & CC)_
	ldx     <apl_length + 1

	ldy     <apl_offset + 1         // If offset <    256_
	beq     _lt256
	cpy     #$7D                    // If offset >= 32000, length += 2_
	bcs     _match_plus2
	cpy     #$05                    // If offset >=  1280, length += 1_
	bcs     _match_plus1
	bcc     _do_match
_lt256:
	ldy     <apl_offset + 0         // If offset <    128, length += 2_
	bmi     _do_match

	sec                             // aPLib gamma returns with CC_

_match_plus2:
	adc     #1                      // CS, so ADC #2_
	bcs     _match_plus256

_match_plus1:
	adc     #0                      // CS, so ADC #1, or CC if fall
	bcc     _do_match               // through from _match_plus2_

_match_plus256:
	inx
_do_match:
	eor     #$FF                    // Negate the lo-byte of length
	tay                             // and check for zero_
	iny
	beq     _calc_addr
	eor     #$FF

	inx                             // Increment # of pages to copy_

	clc                             // Calc destination for partial
	adc     <apl_dstptr + 0         // page_
	sta     <apl_dstptr + 0
	bcs     _calc_addr
	dec     <apl_dstptr + 1

_calc_addr:
	sec                             // Calc address of match_
	lda     <apl_dstptr + 0
	sbc     <apl_offset + 0
	sta     <apl_winptr + 0
	lda     <apl_dstptr + 1
	sbc     <apl_offset + 1
	sta     <apl_winptr + 1

_copy_page:
	lda     (apl_winptr),y
	sta     (apl_dstptr),y
	iny
	bne     _copy_page
	inc     <apl_winptr + 1
	inc     <apl_dstptr + 1
	dex                             // Any full pages left to copy?
	bne     _copy_page

	inx                             // LWM=1_
	jmp     _next_tag

//
// Subroutines for byte & bit handling_
//

_load_bit:
	APL_GET_SRC()                    // Reload an empty bit-buffer
	rol                             // from the compressed source_
	sta     <apl_bitbuf
	rts
}
