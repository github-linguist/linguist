BLARGG_MACROS_INCLUDED = 1

; Allows extra error checking with modified version
; of ca65. Otherwise acts like a constant of 0.
ADDR = 0

; Switches to Segment and places Line there.
; Line can be an .align directive, .res, .byte, etc.
; Examples:
; seg_data BSS, .align 256
; seg_data RODATA, {message: .byte "Test",0}
.macro seg_data Segment, Line
	.pushseg
	.segment .string(Segment)
		Line
	.popseg
.endmacro

; Reserves Size bytes in Segment for Name.
; If Size is omitted, reserves one byte.
.macro seg_res Segment, Name, Size
	.ifblank Size
		seg_data Segment, Name: .res 1
	.else
		seg_data Segment, Name: .res Size
	.endif
.endmacro

; Shortcuts for zeropage, bss, and stack
.define zp_res  seg_res ZEROPAGE,
.define nv_res  seg_res NVRAM,
.define bss_res seg_res BSS,
.define sp_res  seg_res STACK,
.define zp_byte zp_res

; Copies byte from Src to Addr. If Src begins with #,
; it sets Addr to the immediate value.
; Out: A = byte copied
; Preserved: X, Y
.macro mov Addr, Src
	lda Src
	sta Addr
.endmacro

; Copies word from Src to Addr. If Src begins with #,
; it sets Addr the immediate value.
; Out: A = high byte of word
; Preserved: X, Y
.macro movw Addr, Src
	.if .match( .left( 1, {Src} ), # )
		lda #<(.right( .tcount( {Src} )-1, {Src} ))
		sta Addr
		lda #>(.right( .tcount( {Src} )-1, {Src} ))
		sta 1+(Addr)
	.else
		lda Src
		sta Addr
		lda 1+(Src)
		sta 1+(Addr)
	.endif
.endmacro

; Increments 16-bit value at Addr.
; Out: EQ/NE based on resulting 16-bit value
; Preserved: A, X, Y
.macro incw Addr
	.local @Skip
	inc Addr
	bne @Skip
	inc 1+(Addr)
@Skip:
.endmacro

; Adds Src to word at Addr.
; Out: A = high byte of result, carry set appropriately
; Preserved: X, Y
.macro addw Addr, Src
	.if .match( .left( 1, {Src} ), # )
		addw_ Addr,(.right( .tcount( {Src} )-1, {Src} ))
	.else
		lda Addr
		clc
		adc Src
		sta Addr
		
		lda 1+(Addr)
		adc 1+(Src)
		sta 1+(Addr)
	.endif
.endmacro
.macro addw_ Addr, Imm
	lda Addr
	clc
	adc #<Imm
	sta Addr
	
	;.if (Imm >> 8) <> 0
		lda 1+(Addr)
		adc #>Imm
		sta 1+(Addr)
	;.else
	;   .local @Skip
	;   bcc @Skip
	;   inc 1+(Addr)
	;@Skip:
	;.endif
.endmacro

; Splits list of words into tables of low and high bytes
; Example: split_words foo, {$1234, $5678}
; expands to:
; foo_l: $34, $78
; foo_h: $12, $56
; foo_count = 2
.macro split_words Label, Words
	.ident (.concat (.string(Label), "_l")): .lobytes Words
	.ident (.concat (.string(Label), "_h")): .hibytes Words
	.ident (.concat (.string(Label), "_count")) = * - .ident (.concat (.string(Label), "_h"))
.endmacro

.macro SELECT Bool, True, False, Extra
	.ifndef Bool
		False Extra
	.elseif Bool <> 0
		True Extra
	.else
		False Extra
	.endif
.endmacro

.macro DEFAULT Name, Value
	.ifndef Name
		Name = Value
	.endif
.endmacro

.ifp02
	; 6502 doesn't define these alternate names
	.define blt bcc
	.define bge bcs
.endif
.define jlt jcc
.define jge jcs

; Jxx Target = Bxx Target, except it can go farther than
; 128 bytes. Implemented via branch around a JMP.

; Don't use ca65's longbranch, because they fail for @labels
;.macpack longbranch

.macro jeq Target
	bne *+5
	jmp Target
.endmacro

.macro jne Target
	beq *+5
	jmp Target
.endmacro

.macro jmi Target
	bpl *+5
	jmp Target
.endmacro

.macro jpl Target
	bmi *+5
	jmp Target
.endmacro

.macro jcs Target
	bcc *+5
	jmp Target
.endmacro

.macro jcc Target
	bcs *+5
	jmp Target
.endmacro

.macro jvs Target
	bvc *+5
	jmp Target
.endmacro

.macro jvc Target
	bvs *+5
	jmp Target
.endmacro


; Passes constant data to routine in addr
; Preserved: A, X, Y
.macro jsr_with_addr routine,data
	.local Addr
	pha
	lda #<Addr
	sta addr
	lda #>Addr
	sta addr+1
	pla
	jsr routine
	seg_data RODATA,{Addr: data}
.endmacro

; Calls routine multiple times, with A having the
; value 'start' the first time, 'start+step' the
; second time, up to 'end' for the last time.
.macro for_loop routine,start,end,step
	.local @for_loop
	lda #start
@for_loop:
	pha
	jsr routine
	pla
	clc
	adc #step
	cmp #<((end)+(step))
	bne @for_loop
.endmacro

; Calls routine n times. The value of A in the routine
; counts from 0 to n-1.
.macro loop_n_times routine,n
	for_loop routine,0,n-1,+1
.endmacro

; Same as for_loop, except uses 16-bit value in YX.
; -256 <= step <= 255
.macro for_loop16 routine,start,end,step
.if (step) < -256 || (step) > 255
	.error "Step must be within -256 to 255"
.endif
	.local @for_loop_skip
	.local @for_loop
	ldy #>(start)
	lda #<(start)
@for_loop:
	tax
	pha
	tya
	pha
	jsr routine
	pla
	tay
	pla
	clc
	adc #step
.if (step) > 0
	bcc @for_loop_skip
	iny
.else
	bcs @for_loop_skip
	dey
.endif
@for_loop_skip:
	cmp #<((end)+(step))
	bne @for_loop
	cpy #>((end)+(step))
	bne @for_loop
.endmacro

; Stores byte at addr
; Preserved: X, Y
.macro setb addr, byte
	lda #byte
	sta addr
.endmacro

; Stores word at addr
; Preserved: X, Y
.macro setw addr, word
	lda #<(word)
	sta addr
	lda #>(word)
	sta addr+1
.endmacro

; Loads XY with 16-bit immediate or value at address
.macro ldxy Arg
	.if .match( .left( 1, {Arg} ), # )
		ldy #<(.right( .tcount( {Arg} )-1, {Arg} ))
		ldx #>(.right( .tcount( {Arg} )-1, {Arg} ))
	.else
		ldy (Arg)
		ldx (Arg)+1
	.endif
.endmacro

; Increments XY as 16-bit register, in CONSTANT time.
; Z flag set based on entire result.
; Preserved: A
; Time: 7 clocks
.macro inxy
	iny ; 2
	beq *+4 ; 3
		; -1
	bne *+3 ; 3
		; -1
	inx ; 2
.endmacro

; Negates A and adds it to operand
.macro subaf Operand
	eor #$FF
	sec
	adc Operand
.endmacro

; Initializes CPU registers to reasonable values
; Preserved: A, Y
.macro init_cpu_regs
	sei
	cld     ; unnecessary on NES, but might help on clone
	ldx #$FF
	txs
	.ifndef BUILD_NSF
		inx
		stx PPUCTRL
	.endif
.endmacro
