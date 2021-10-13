;;; Created by Miguel Mendez.
;;; This file is public domain.
    
    machine 68060

	section text
	public _getCPU
	public _getVBR
	public _getMEM

LVOSuperVisor EQU -30
LVOAvailMem EQU -216
AttnFlags EQU 296
	
_getCPU:
	movem.l d1/a6, -(sp)
	move.l  4.w, a6          ; exec base.
        move.w  AttnFlags(a6), d1     ; AttnFlags
        
        btst    #0, d1
        bne.s   _010            ; 68010
        moveq   #0, d0
        bra.b   _cpuDone

_010    btst    #1, d1
        bne.s   _020            ; 68020
        moveq   #1,d0
        bra.b   _cpuDone

_020    btst    #2, d1
        bne.s   _030            ; 68030
        moveq   #2, d0
        bra.b   _cpuDone
_030    btst    #3, d1
        bne.s   _040            ; 68040
        moveq   #3, d0
        bra.b   _cpuDone
_040    btst    #7, d1
        bne.s   _060            ; 68060
        moveq   #4, d0
        bra.b   _cpuDone
_060	btst    #10, d1         ; Apollo 68080
	bne.s   _080
	moveq   #6, d0
	bra.b   _cpuDone
_080	moveq   #8, d0
_cpuDone:
	movem.l (sp)+, d1/a6
	rts
	
_getVBR:
	movem.l a5-a6, -(sp)
	move.l $4.w, a6
	lea.l movecTrap, a5
	jsr LVOSuperVisor(a6)
	movem.l (sp)+, a5-a6
	rts

movecTrap:
	movec.l VBR, d0
	rte
	

_getMEM:
	movem.l a6, -(sp)
	move.l  4.w,a6
	moveq #0, d0
        jsr     LVOAvailMem(a6)
	movem.l (sp)+, a6
	rts
