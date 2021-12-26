;----------------------------------------------------------------------
; CMDR-DOS Directory Listing
;----------------------------------------------------------------------
; (C)2020 Michael Steil, License: 2-clause BSD

.export dir_open, dir_close, dir_read

; cmdch.s
.import set_status
.import bin_to_bcd

; file.s
.import set_errno_status, convert_errno_status

; functions.s
.import create_fat32_path_only_dir, create_fat32_path_only_name
.import alloc_context, alloc_context2, free_context

.import parse_dos_filename
.import buffer
.import file_type, filter0, filter1, medium

.include "fat32/fat32.inc"
.include "fat32/regs.inc"

DIRSTART = $0801 ; load address of directory

.bss

dirbuffer:
	.res 256, 0

dirbuffer_r:
	.byte 0
dirbuffer_w:
	.byte 0
num_blocks:
	.word 0
context:
	.byte 0
dir_eof:
	.byte 0
part_index:
	.byte 0
show_timestamps:
	.byte 0

.code

;---------------------------------------------------------------
;---------------------------------------------------------------
dir_open:
	pha ; filename length

	lda #0
	jsr set_status

	stz show_timestamps

	ply ; filename length
	lda #0
	sta buffer, y ; zero terminate

	lda #$80
	sta part_index ; flag: files index, not partition index

	lda buffer+1
	cmp #'='
	bne @files_dir
	lda buffer+2
	cmp #'P'
	beq @part_dir
	cmp #'T'
	bne @files_dir
	lda #$80
	sta show_timestamps
	ldx #3 ; skip "=T"
	bra @cont1

@part_dir:
	stz part_index

	; partition directory
	lda buffer+3
	cmp #':'
	bne @no_filter

	ldx #4
	bra @cont1

@no_filter:
	ldx #1
	ldy #1
	bra @cont1

@files_dir:

	ldx #1
@cont1:
	jsr parse_dos_filename
	bcc @1
	lda #$30 ; syntax error
	jmp @dir_open_err
@1:	

	bit part_index
	bmi @not_part0
	lda #$ff
	jsr alloc_context2
	bra @cont0

@not_part0:
	jsr alloc_context
@cont0:
	bcs @alloc_ok
	jsr set_errno_status
	sec
	rts

@alloc_ok:
	sta context
	jsr fat32_set_context

	ldy #0
	lda #<DIRSTART
	jsr storedir
	lda #>DIRSTART
	jsr storedir
	lda #1
	jsr storedir ; link
	jsr storedir

	lda medium
	bit part_index
	bmi @not_part1
	lda #255
@not_part1:
	jsr storedir ; header line number
	lda #0
	jsr storedir
	lda #$12 ; reverse
	jsr storedir
	lda #$22 ; quote
	jsr storedir

	bit part_index
	bmi @not_part2

	ldx #txt_part_dir_header - txt_tables
	jsr storetxt
	ldx #txt_part_dir_header_end - txt_part_dir_header
	bra @4

@not_part2:
	phy
	jsr fat32_get_vollabel
	ply
	bcc @dir_open_err3

	ldx #0
@3:	lda fat32_dirent + dirent::name,x
	beq @4
	jsr storedir
	inx
	bne @3

@4:	cpx #16
	bcs @5
	lda #$20
	jsr storedir
	inx
	bra @4
@5:

	lda #$22 ; quote
	jsr storedir
	lda #' '
	jsr storedir

	ldx #txt_fat32 - txt_tables
	bit part_index
	bmi @not_part4
	ldx #txt_mbr - txt_tables
@not_part4:
	jsr storetxt
	lda #0 ; end of line
	jsr storedir
	phy

	jsr create_fat32_path_only_dir

	bit part_index
	bpl @cont3

	jsr fat32_open_dir
@cont3:
	ply
	bcc @dir_open_err3

	sty dirbuffer_w
	stz dirbuffer_r

	stz dir_eof
	clc ; ok
	rts

@dir_open_err3:
	jsr set_errno_status
	bra @dir_open_err2
@dir_open_err:
	jsr set_status
@dir_open_err2:
	lda context
	jsr free_context
	lda #1
	sta dir_eof
	clc ; ok
	rts

;---------------------------------------------------------------
;---------------------------------------------------------------
dir_read:
	ldx dirbuffer_r
	cpx dirbuffer_w
	beq @acptr_empty

	lda dirbuffer,x
	inc dirbuffer_r
	clc
	rts

@acptr_empty:
	jsr read_dir_entry
	bcc dir_read
	lda #0
	rts ; C = 1


;---------------------------------------------------------------
read_dir_entry:
	lda dir_eof
	beq @read_entry
@error:
	sec
	rts

@read_entry:
	; XXX this assumes the filename or command are not
	; XXX overwritten while reading a directory
	jsr create_fat32_path_only_name

	bit part_index
	bmi @not_part1
	lda part_index
	inc part_index
	cmp #4
	beq @dir_end2
	jsr fat32_get_ptable_entry
	bcc @error
	lda fat32_dirent + dirent::attributes
	beq @read_entry
	bra @cont1

@not_part1:
	jsr fat32_read_dirent_filtered
@cont1:
	bcs @found
	lda fat32_errno
	beq :+
	jsr set_errno_status
:	bit part_index
	bmi :+
@dir_end2:
	jmp @dir_end
:	jmp @read_dir_entry_end

@found:
; in partition mode, don't evaluate "attributes" (it's the part type!)
	bit part_index
	bmi @show3

; Skip hidden entries unless option 'A' ("ALL") is given
	lda fat32_dirent + dirent::attributes
	and #2
	beq @show1
	lda #'A'
	jsr has_filter
	bne @read_entry
@show1:

	; Skip files if type is 'D' ("DIR")
	lda fat32_dirent + dirent::attributes
	and #$10
	bne @show2
	lda #'D'
	jsr has_filter
	beq @read_entry
@show2:

	; Skip directories if type is 'P' ("PRG")
	lda fat32_dirent + dirent::attributes
	and #$10
	beq @show3
	lda #'P'
	jsr has_filter
	beq @read_entry
@show3:

	ldy #0
	lda #1
	jsr storedir ; link
	jsr storedir

	lda part_index
	bmi @file1

	sta num_blocks
	stz num_blocks + 1
	bra @file1_cont

@file1:	tya
	tax
	lda fat32_dirent + dirent::size + 0
	clc
	adc #255
	lda #0
	adc fat32_dirent + dirent::size + 1
	sta num_blocks
	lda #0
	adc fat32_dirent + dirent::size + 2
	sta num_blocks + 1
	lda #0
	adc fat32_dirent + dirent::size + 3
	beq :+
	lda #$ff ; overflows 65535 blocks, so show 65535
	sta num_blocks
	sta num_blocks + 1
:	txa
	tay

@file1_cont:
	lda num_blocks
	jsr storedir
	lda num_blocks + 1
	jsr storedir

	; find out how many spaces to print
	lda num_blocks
	sec
	sbc #<10000
	lda num_blocks + 1
	sbc #>10000
	bcc @ngt_10000
	lda #'T' - $40
	jsr storedir
	bra @gt_1000

@ngt_10000:
	lda num_blocks
	sec
	sbc #<1000
	lda num_blocks + 1
	sbc #>1000
	bcs @gt_1000

	lda num_blocks
	sec
	sbc #<100
	lda num_blocks + 1
	sbc #>100
	bcs @gt_100

	lda num_blocks
	sec
	sbc #<10
	lda num_blocks + 1
	sbc #>10
	bcs @gt_10

	ldx #2
	bra :+
@gt_10:
	ldx #1
	bra :+
@gt_100:
	ldx #0
:	lda #' '
:	jsr storedir
	dex
	bpl :-
@gt_1000:

;@gt_10000:
	lda #$22
	jsr storedir

	ldx #0
:	lda fat32_dirent + dirent::name, x
	beq :+
	jsr storedir
	inx
	bne :-
:
	lda #$22 ; quote
	jsr storedir

	lda #' '
:	jsr storedir
	inx
	cpx #17
	bcc :-

	lda fat32_dirent + dirent::attributes

	bit part_index
	bmi @not_part2
	ldx #txt_fat32 - txt_tables
	cmp #$0b
	beq @c1
	cmp #$0c
	beq @c1
	ldx #txt_exfat - txt_tables
	cmp #$07
	beq @c1
	pha
	lda #'T'
	jsr storedir
	pla
	jsr storehex8
	bra @read_dir_eol
@c1:	jsr storetxt
	bra @read_dir_eol

@not_part2:
	ldx #txt_prg - txt_tables
	bit #$10 ; = directory
	beq @read_dir_cont
	ldx #txt_dir - txt_tables
@read_dir_cont:
	jsr storetxt

	lda fat32_dirent + dirent::attributes
	lsr
	lda #' '
	bcc :+
	lda #'<' ; write protect indicator
:	jsr storedir

@read_dir_eol:
	bit part_index
	bpl @not_part3

	bit show_timestamps
	bpl @not_part3

	; timestamp
	lda fat32_dirent + dirent::mtime_year
	cmp #$ff
	beq @not_part3 ; no timestamp
	pha
	lda #' '
	jsr storedir
	pla
	cmp #20
	bcs @tim1
	pha
	lda #'1'
	jsr storedir
	lda #'9'
	jsr storedir
	pla
	clc
	adc #80
	bra @tim2
@tim1:	pha
	lda #'2'
	jsr storedir
	lda #'0'
	jsr storedir
	pla
	sec
	sbc #20
@tim2:	jsr storedec8
	lda #'-'
	jsr storedir
	lda fat32_dirent + dirent::mtime_month
	jsr storedec8
	lda #'-'
	jsr storedir
	lda fat32_dirent + dirent::mtime_day
	jsr storedec8
	lda #' '
	jsr storedir
	lda fat32_dirent + dirent::mtime_hours
	jsr storedec8
	lda #':'
	jsr storedir
	lda fat32_dirent + dirent::mtime_minutes
	jsr storedec8
	lda #':'
	jsr storedir
	lda fat32_dirent + dirent::mtime_seconds
	jsr storedec8
	lda #' '
	jsr storedir

@not_part3:

	lda #0 ; end of line
	jsr storedir

	stz dir_eof

	sty dirbuffer_w
	stz dirbuffer_r
	clc ; ok
	rts


@read_dir_entry_end:
	ldy #0
	lda #1
	jsr storedir ; link
	jsr storedir

	jsr fat32_get_free_space
	lda fat32_size + 2
	ora fat32_size + 3
	bne @not_kb

	lda #'K'
	bra @print_free

@not_kb:
	jsr shr10
	lda fat32_size + 2
	bne @not_mb

	lda #'M'
	bra @print_free

@not_mb:
	jsr shr10
	lda #'G'

@print_free:
	pha
	lda fat32_size + 0
	jsr storedir
	lda fat32_size + 1
	jsr storedir
	pla
	jsr storedir

	ldx #txt_free - txt_tables
	jsr storetxt

@dir_end:
	lda #0
	jsr storedir
	jsr storedir
	; the final 0 is missing, because the character transmission
	; function will send one extra 0 with EOI

	inc dir_eof ; = 1

	sty dirbuffer_w
	stz dirbuffer_r
	clc ; ok
	rts

has_filter:
	cmp filter0
	beq @yes
	cmp filter1
@yes:	rts

;---------------------------------------------------------------
dir_close:
	jsr fat32_close ; can't fail
	lda context
	jmp free_context

;---------------------------------------------------------------
txt_tables:
txt_free:
	.byte "B FREE.", 0
txt_part_dir_header:
	.byte "CMDR-DOS SD CARD", 0
txt_part_dir_header_end:
txt_mbr:
	.byte " MBR ", 0
txt_fat32:
	.byte "FAT32", 0
txt_exfat:
	.byte "EXFAT", 0
txt_prg:
	.byte "PRG", 0
txt_dir:
	.byte "DIR", 0

storedec8: ; supports one or two digits only; 0-padded
	jsr bin_to_bcd
storehex8:
	pha
	lsr
	lsr
	lsr
	lsr
	jsr storehex4
	pla
storehex4:
	and #$0f
	cmp #$0a
	bcc :+
	adc #$66
:	eor #$30
	jsr storedir
	rts

storetxt:
	lda txt_tables,x
	beq @1
	jsr storedir
	inx
	bne storetxt
@1:	rts

storedir:
	sta dirbuffer,y
	iny
	rts

shr10:
	; >> 8
	lda fat32_size + 1
	sta fat32_size + 0
	lda fat32_size + 2
	sta fat32_size + 1
	lda fat32_size + 3
	sta fat32_size + 2
	stz fat32_size + 3

	; >> 2
	lsr fat32_size + 2
	ror fat32_size + 1
	ror fat32_size + 0
	lsr fat32_size + 2
	ror fat32_size + 1
	ror fat32_size + 0

	rts
