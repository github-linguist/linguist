;;; Created by Miguel Mendez.
;;; This file is public domain.
	
	section text
	public _installLevel2
	public _installLevel3
	public _ciab_start
	public _ciab_stop
	public _closeOS
	public _getKeyPress
	public _restoreOS
	public _getMasterTimer
	public _resetMasterTimer
	public _mouse_left
	public _mouse_right
	public _mousePressL
	public _mousePressR
	public _mousePosX
	public _mousePosY
	public _serialPutc
	
LVOOpenLibrary EQU -552
LVOCloseLibrary EQU -414	
LVOWaitBlitt EQU -228
LVODisownBlitter EQU -462 
LVOOwnBlitter EQU -456
LVOLoadView EQU -222
LVOWaitTOF EQU	-270
LVOForbid EQU -132
LVOPermit EQU -138
SERDAT EQU $dff030
SERDATR EQU $dff018
CIAB EQU $bfd000
CIAA EQU $bfe001
CIASDR EQU $0C00
CIAA_CRA EQU $e00
CIAB_CRA EQU $e00
CIAA_ICR EQU $d00
CIAB_ICR EQU $d00
CIAB_TALO EQU $400
CIAB_TAHI EQU $500
CIAICRB_SP	  EQU	3
INTB_PORTS     EQU   (3)
INTF_PORTS     EQU   (1<<3)
INTREQR EQU $1e
INTREQ EQU $9c
CIACRAF_SPMODE	  EQU	(1<<6)
VHPOSR	    EQU   $006

	
_closeOS:
	movem.l	d0-a6, -(sp)
	move.l  $6c(a0),_int3save               ; store original vblank int
	move.l	$68(a0),_int2save		; store original level 2 int
	
	lea     gfx_lib,a1      ; open graphics.library
        moveq   #0,d0
        move.l  4.w,a6          ; exec base
        jsr     LVOOpenLibrary(a6)

        move.l  d0,_gfxbase     ; store open gfxlibrary pointer
        move.l  d0,a6
        move.l  38(a6),_oldcopper       ; store old copper list address
        move.l  34(a6),_oldview         ; store old WB view.

        jsr     LVOWaitBlitt(a6)
        jsr     LVOOwnBlitter(a6)        ; _LVOOwnBlitter                
        jsr     LVOWaitBlitt(a6)        ; _LVOWaitBlitt

        moveq   #0,d7
        move.l  d7,a1

        jsr     LVOLoadView(a6)        ; _LVOLoadView(Null)
        jsr     LVOWaitTOF(a6)        ; _LVOWaitTOF
        jsr     LVOWaitTOF(a6)        ; _LVOWaitTOF

	move.l  4.w,a6
        jsr     LVOForbid(a6)        ; _LVOForbid
	
        move.w    $DFF01C,_intenar
        move.w    $DFF01E,_intreqr
        move.w    $DFF002,_dmaconr
        move.w    $DFF010,_adkconr

	move.w    #%0111111111111111,$DFF09A      ; Intena

	lea	CIAA, a4	; save CIAA registers and setup
	move.b	CIAA_CRA(a4), _ciaa_cra
	move.b 	CIAA_ICR(a4), _ciaa_icr
	
	lea     CIAB, a4	; save CIAB registers and setup 
	move.b  CIAB_CRA(a4), _ciab_cra
	move.b  CIAB_ICR(a4), _ciab_icr

	move.b  _ciab_cra, d0
	and.b   #%11000000,d0           ;Don't trash bits we are not
        or.b    #%00001000,d0           ;using...
	move.b  d0, CIAB_CRA(a4)
	move.b  #%01111111, CIAB_ICR(a4)
	move.b  CIAB_TALO(a4), _ciab_ta_lo
	move.b  CIAB_TAHI(a4), _ciab_ta_hi
	
	move.w    #%1110000000100000,$DFF09A      ; enable needed IRQs

        move.l  _gfxbase,a6
        jsr     LVOWaitTOF(a6)        ; WaitTOF

	move.w    #%0111111111111111,$DFF096      ; DMACONW
	move.w    #%1000001111000000,$DFF096

	;; bsr	_setupFPCR

	movem.l	(sp)+,d0-a6
	moveq #0, d0
	rts

_restoreOS:
	movem.l	d0-a6,-(sp)

	;; bsr	_restoreFPCR

	lea	CIAA, a4	; restore CIAA registers
	move.b	_ciaa_cra, CIAA_CRA(a4)
	nop
	move.b	_ciaa_icr, CIAA_ICR(a4)
	nop
	
	lea     CIAB, a4    ; restore CIAB registers
	move.b  _ciab_ta_lo, CIAB_TALO(a4)
	nop
	move.b  _ciab_ta_hi, CIAB_TAHI(a4)
	nop
	move.b  _ciab_cra, CIAB_CRA(a4)
	nop
	move.b  _ciab_icr, CIAB_ICR(a4)
	nop
	
	move.l  _int3save,$6c(a0)       ; restore old level3 int.

	move.l	_int2save,$68(a0) 	;restore old level2 int.

        move    _intenar,d7
        bset    #$f,d7
        move    d7,$DFF09A              ; restore old interrupts.


        move    _intreqr,d7
        bset    #$f,d7
        move    d7,$DFF09C              ; the same...   
        
        move    _dmaconr,d7
        bset    #$f,d7
        move    d7,$DFF096              ; restore old DMAs

        move    _adkconr,d7
        bset    #$f,d7
        move    d7,$DFF09E              ; restore Adkcon

        move.l  _gfxbase,a6
        move.l  _oldview,a1
        jsr     LVOLoadView(a6)         ; _LVOLoadview(OldView)

        move.l  _oldcopper,$dff080      ; restore old copper list.

	jsr     LVODisownBlitter(a6)    ; _LVODisownBlitter

        move.l  4.w,a6
        move.l  _gfxbase,a1
        jsr     LVOCloseLibrary(a6)     ; close graphics.library

        jsr     LVOPermit(a6)           ; _LVOPermit
	movem.l	(sp)+,d0-a6
	moveq #0, d0
        rts

; Clear DZ bit --> return infinity when dividing by zero.
; Infinity is supported in hardware in 040 and 060,
; and the register can be read and written in user mode.
; See pages 6-3,  6-4, 6-7 and 6-32 in 68060 manual.
_setupFPCR:
	fmove.l	fpcr,d0
	move.l	d0,_fpcr
	; clear DA
	not.l	d0
	or.l	#$00000400,d0
	not.l	d0
	; set round-to-zero (01)
	not.l	d0
	or.l	#$00000030,d0
	not.l	d0
	or.l	#$00000010,d0
	; set always single (01)
	;not.l	d0
	;or.l	#$000000c0,d0
	;not.l	d0
	;or.l	#$00000040,d0
	fmove.l	d0,fpcr
	rts

_restoreFPCR:
	move.l	_fpcr,d0
	fmove.l	d0,fpcr
	moveq.l	#0,d0
	rts

_installLevel3:
	move.l a1, _vblcallback
	move.l a2, _vblcallback2
	lea Level3(pc), a1
	move.l a1, $6c(a0)
	rts

_installLevel2:
	lea Level2(pc), a1
	
	move.l a1, $68(a0)
	nop
	nop
	move.w    #%1000000000001000,$DFF09A ; enable level2 IRQ
	rts
	
_mousePressL:
	moveq.l	#0,d0
	btst	#6,$bfe001
	seq	d0
	rts

_mousePressR:
	moveq.l	#0,d0
	btst	#10,$dff016
	seq	d0
	rts

_mousePosX:
_mousePosY:
    moveq   #0,d0
    rts

Level3:
	movem.l	d0-d7/a0-a6,-(sp)
	lea $dff000,a6
	move.w $dff01e,d0 ; intreq-read
	btst #5,d0 ; Vertical Blanc?
	bne .vertb
	btst #6,d0 ; Blitter
	bne .blit
	bra .quitL3

;--- Blitter
.blit:
	move.w #$4040,$dff09c
	move.w #$4040,$dff09c
	bra .quitL3

;--- Vertical Blank
.vertb:
	addq.l #1, _master_timer ; increase timer by 1
	move.w #$4020,$dff09c
	move.w #$4020,$dff09c
	move.l _vblcallback, a5	; Paula replay callback.
    cmp.l   #0,a5
    beq     .skip_callback1
	jsr (a5)
.skip_callback1
	move.l _vblcallback2, a5 ; Lerp callback.
    cmp.l   #0,a5
    beq     .quitL3
	jsr (a5)
	bra.w .quitL3
.quitL3:
	movem.l	(sp)+,d0-d7/a0-a6
	nop
	nop
	rte 
	
Level2:
	movem.l d0-d1/a0-a2, -(sp)
	lea $dff000,a0
	move.w INTREQR(a0), d0
	btst #INTB_PORTS, d0
	beq l2_end

	lea CIAA, a1
	btst #CIAICRB_SP, CIAA_ICR(a1)
	beq l2_end

	move.b	CIASDR(a1),d0
	or.b	#CIACRAF_SPMODE,CIAA_CRA(a1)
	not.b	d0
	ror.b	#1,d0
	move.b  d0, _rawkey
	
	;handshake
	moveq	#3-1,d1
.wait1	move.b	VHPOSR(a0),d0
.wait2	cmp.b	VHPOSR(a0),d0
	beq	.wait2
	dbf	d1,.wait1

	;set input mode
	and.b	#~(CIACRAF_SPMODE),CIAA_CRA(a1)

l2_end:	move.w	#INTF_PORTS, INTREQ(a0)
	tst.w	INTREQR(a0)
	movem.l	(sp)+,d0-d1/a0-a2
	nop
	nop
	rte

_getKeyPress:
	moveq 	#0, d0
	moveq   #0, d1
	move.b  _rawkey, d0
	move.b  d1, _rawkey
	rts
	
_getMasterTimer:
	move.l  _master_timer,d0
	rts

_resetMasterTimer:
	moveq.l #0,d0
	move.l d0, _master_timer
	rts

_serialPutc:
	move.w SERDATR, d1
	btst   #13, d1
	beq.s  _serialPutc  	; Wait for character to finish
	and.b  #$7f, d1
	cmp.b  #$18, d1 	; Ctrl-X?
	beq.s  spc_exit
	cmp.b  #$13, d1		; Ctrl-S?
	beq.s  _serialPutc
	and.w  #$ff, d0
	or.w   #$100, d0
	move.w d0, SERDAT
spc_exit:
	rts

_ciab_start:
	lea CIAB, a0
	move.b #$ff, CIAB_TALO(a0)
	nop
	move.b #$ff, CIAB_TAHI(a0)
	nop
	bset.b #0, CIAB_CRA(a0)
	rts

_ciab_stop:
	lea    CIAB, a0
	bclr.b #0, CIAB_CRA(a0)
	moveq #0, d0
	move.b CIAB_TAHI(a0), d0
	lsl.w #8, d0
	move.b CIAB_TALO(a0), d0
	rts
	
	section data, data

gfx_lib         dc.b "graphics.library",0,0
_vblcallback	dc.l	0
_vblcallback2	dc.l	0	
_vbr  	 	dc.l 	0	
_fpcr		dc.l	0
_gfxbase        dc.l    0
_oldcopper      dc.l    0
_oldview        dc.l    0
_intenar        dc.w    0
_intreqr        dc.w    0
_dmaconr        dc.w    0
_adkconr        dc.w    0
_int2save	dc.l	0	
_int3save       dc.l    0
_master_timer	dc.l	0
_mouse_left	dc.l	0
_mouse_right	dc.l	0	
_ciab_ta_lo     dc.b    0
_ciab_ta_hi     dc.b    0
_ciab_icr       dc.b    0
_ciaa_icr	dc.b	0	
_ciab_cra       dc.b    0
_ciaa_cra	dc.b	0	
_rawkey 	dcb.b 	0
