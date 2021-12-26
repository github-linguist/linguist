	.PC02


; Includes
	.include	"registers.inc"
	.include	"vera.inc"
	.include	"kernal.inc"

; Imports
	.import draw_counter
	.import draw_button
	.import	draw_field
	.import	config_screen_setup

	.import	gfx_minesweeper_tilesheet
	.import	gfx_pointer

; Constants
	.export	TILEMAP
	NUMBER			=	0
	TILEMAP			=	$4000
	TILESHEET		=	$5000
	POINTER			=	$7000


	.code
	.export	load_tilesheet
; Loads the minesweeper tilesheet into VRAM starting at $5000
.proc	load_tilesheet
	lda	#0			; Use data port 0
	sta	vera::CTRL
	lda	#<TILESHEET		; Load at $5000 in VRAM
	sta	vera::ADDR0_L
	lda	#>TILESHEET
	sta	vera::ADDR0_M
	lda	#$10			; Increment by 1
	sta	vera::ADDR0_H
	lda	#<gfx_minesweeper_tilesheet	; Load from the data area
	sta	r11
	lda	#>gfx_minesweeper_tilesheet
	sta	r11+1
loop:
	lda	(r11)			; Load one byte of tilesheet data
	sta	vera::DATA0		; Transfer it to VRAM
	inc	r11			; Increment low byte of memory address
	bne	check			; If the low byte is not equal to 0, skip to check
	inc	r11+1			; Increment high byte of memory address
check:
	lda	#<gfx_minesweeper_tilesheet
	cmp	r11			; Is the low byte equal to its final value?
	bne	loop			; If not, loop again
	lda	#>gfx_minesweeper_tilesheet+$20
	cmp	r11+1			; Is the high byte equal to its final value?
	bne	loop			; If not, loop again
	rts
.endproc


	.export	setup_screen
; Draws the timer, mine counter, and reset button to layer 0
.proc	setup_screen
	lda	#$02			; Set screen mode to 80x60 text
	jsr	kernal::screen_set_mode
	jsr	setup_layer_0		; Setup video layer 0
	lda	vera::DC_VIDEO		; Enable layer 0, disable layer 1
	ora	#$10
	and	#<~$20
	sta	vera::DC_VIDEO
	jsr	clear_layer_0		; Clear layer 0
	jsr	draw_counter_outlines	; Draw the outlines for the counters
	lda	#$2a			; Draw the reset button
	jsr	draw_button
	jsr	config_screen_setup	; Setup layer 1
	rts
.endproc


; Sets up the registers controlling video layer 0
.proc	setup_layer_0
	lda	#$12			; 64x32 tilemap, 4bpp
	sta	vera::L0_CONFIG
	lda	#>TILEMAP >> 1		; Set tilemap base address to $4000
	sta	vera::L0_MAPBASE
	lda	#>TILESHEET >> 1 | 3	; Set tilesheet base address to $5000 and tile size to 16x16
	sta	vera::L0_TILEBASE
	rts
.endproc


	.export	load_palette
; Loads the palette into VRAM.
.proc	load_palette
	lda	#0			; Use data port 0
	sta	vera::CTRL
	lda	#<vera::PALETTE		; Load at $1fa00 in VRAM
	sta	vera::ADDR0_L
	lda	#>vera::PALETTE
	sta	vera::ADDR0_M
	lda	#$10 | ^vera::PALETTE	; Increment by 1
	sta	vera::ADDR0_H
	ldx	#0			; Clear loop counter
loop:
	lda	palette,x		; Get a byte of palette data
	sta	vera::DATA0		; Store it in VRAM
	inx
	cpx	#26			; Have we copied all data?
	bne	loop			; If not, loop again
	rts
.endproc


; Clears layer 0
.proc	clear_layer_0
	lda	#0			; Use data port 0
	sta	vera::CTRL
	lda	#<TILEMAP		; Access VRAM beginning at $4000
	sta	vera::ADDR0_L
	lda	#>TILEMAP
	sta	vera::ADDR0_M
	lda	#$10			; Increment by 1
	sta	vera::ADDR0_H
	ldy	#0			; Initialize loop counter
	ldx	#0
	lda	#0
loop:
	sta	vera::DATA0		; Write tile data
	sta	vera::DATA0
	iny
	bne	loop
	inx
	cpx	#$08			; Have we filled the entire tilemap?
	bne	loop			; If not, loop
	rts
.endproc


; Draws the outlines for the mine counter and the timer
.proc	draw_counter_outlines
	lda	#0			; Use data port 0
	sta	vera::CTRL
	lda	#<TILEMAP		; Access first location
	sta	vera::ADDR0_L
	lda	#>TILEMAP
	sta	vera::ADDR0_M
	lda	#$20			; Increment by 2
	sta	vera::ADDR0_H
	jsr	draw_counter_outline
	lda	#<(TILEMAP+35*2)	; Access second location
	sta	vera::ADDR0_L
	lda	#>(TILEMAP+35*2)
	sta	vera::ADDR0_M
	lda	#$20			; Increment by 2
	sta	vera::ADDR0_H
	jsr	draw_counter_outline
.endproc


; Draws a counter outline. DATA0 must point to a valid tile entry, and increment
; must be set to 2.
.proc	draw_counter_outline
	lda	#$08			; Draw outline corner
	sta	vera::DATA0
	lda	#$01			; Draw outline top
	sta	vera::DATA0
	sta	vera::DATA0
	sta	vera::DATA0
	lda	#$80			; Change increment to 128
	sta	vera::ADDR0_H
	lda	#$05			; Draw outline corner
	sta	vera::DATA0
	lda	#$02			; Draw outline side
	sta	vera::DATA0
	sta	vera::DATA0
	lda	#$28			; Decrement by 2
	sta	vera::ADDR0_H
	lda	#$06			; Draw corner
	sta	vera::DATA0
	lda	#$03			; Draw outline bottom
	sta	vera::DATA0
	sta	vera::DATA0
	sta	vera::DATA0
	lda	#$88			; Decrement by 128
	sta	vera::ADDR0_H
	lda	#$07			; Draw corner
	sta	vera::DATA0
	lda	#$04			; Draw outline side
	sta	vera::DATA0
	sta	vera::DATA0
	rts
.endproc


	.export	load_pointer
.proc	load_pointer
	lda	#0			; Use data port 0
	sta	vera::CTRL
	lda	#<POINTER		; Access VRAM
	sta	vera::ADDR0_L
	lda	#>POINTER
	sta	vera::ADDR0_M
	lda	#$10			; Increment by 1
	sta	vera::ADDR0_H
	ldx	#0			; Initialize loop counter
loop:
	lda	gfx_pointer,x		; Get a byte of sprite data
	sta	vera::DATA0		; Transfer it to VRAM
	inx
	cpx	#128			; Have we transferred all data?
	bne	loop			; If not, loop again
	rts
.endproc


	.export	setup_pointer
.proc	setup_pointer
	lda	#$01			; Pointer is visible
	ldx	#1			; Scale 1
	jsr	kernal::mouse_config
	lda	#0			; Use data port 0
	sta	vera::CTRL
	lda	#<vera::SPRITES		; Access the sprite registers
	sta	vera::ADDR0_L
	lda	#>vera::SPRITES
	sta	vera::ADDR0_M
	lda	#$10 | ^vera::SPRITES	; Increment by 1
	sta	vera::ADDR0_H
	lda	#<(POINTER >> 5)	; Load address of custom pointer
	sta	vera::DATA0
	lda	#>(POINTER >> 5)
	sta	vera::DATA0
	rts
.endproc


	.data
palette:
	.word	$0000, $0000, $0fff, $0777
	.word	$0bbb, $000b, $00b0, $0b00
	.word	$0208, $0802, $0088, $0ee0
	.word	$0200
