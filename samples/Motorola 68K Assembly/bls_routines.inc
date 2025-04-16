; Fill memory with the same word value
BLSFASTFILL_WORD macro dest, words, value
    if words == 0
    elsif words == 1
    move.w  dest, value
    else
    lea     dest, a1
    if words <= 128
    clr     d0
    moveq   words - 1, d0
    else
    move.w  words - 1, d0
    endif
.fastfill\?
    move.w  value, (a0)+
    dbra    d0, .fastfill\?
    endif
    endm

BLSFASTFILL macro dest, value, bytes
        if bytes > 0
        if bytes == 1
         if value == 0
                clr.b   dest
         else
                move.b  #value, dest
         endif
        elsif bytes == 2
         if value == 0
                clr.w   dest
         else
                move.w  #(value<<8) | value, dest
         endif
        elsif bytes == 4
         if value == 0
                clr.l   dest
         else
                move.l  #(value<<24)|(value<<16)|(value<<8)|value, dest
         endif
        else
                lea     dest + bytes, a0
         if bytes == 3 && value == 0
                clr.w   -(a0)
                clr.b   -(a0)
         else

          if value == 0
                moveq   #0, d0
          else
                movei.l #(value<<24)|(value<<16)|(value<<8)|value, d0
          endif

          if bytes & 1
                move.b  d0, -(a0)
          endif

          if bytes < 32
                BLSREPEAT2 bytes / 4, move.l d0, -(a0)
                BLSREPEAT2 (bytes / 2) * 2 - (bytes / 4) * 4, move.w d0, -(a0)
          elsif bytes < 128
                move.l  d0, a1
                move.l  d0, d1
                BLSREPEAT2 bytes / 12, movem.l d0-d1/a1, -(a0)
           if (bytes % 12) >= 10
                movem.l d0-d1, -(a0)
                move.w  d0, -(a0)
           elsif (bytes % 12) >= 8
                movem.l d0-d1, -(a0)
           elsif (bytes % 12) >= 6
                move.l  d0, -(a0)
                move.w  d0, -(a0)
           elsif (bytes % 12) >= 4
                move.l  d0, -(a0)
           elsif (bytes % 12) >= 2
                move.w  d0, -(a0)
           endif
          else
           if bytes & 2
                move.w  d0, -(a0)
           endif

                movem.l d2-d7, -(sp)
                move.l  d0, a1
                move.l  d0, d2
                move.l  d0, d3
                move.l  d0, d4
                move.l  d0, d5
                move.l  d0, d6
                move.l  d0, d7
           if bytes < 512
                BLSREPEAT2 bytes / 32, movem.l d0/a1/d2-d7, -(a0)
            if (bytes % 32) >= 28
                movem.l d0/a1/d2-d6, -(a0)
            elsif (bytes % 32) >= 24
                movem.l d0/a1/d2-d5, -(a0)
            elsif (bytes % 32) >= 20
                movem.l d0/a1/d2-d4, -(a0)
            elsif (bytes % 32) >= 16
                movem.l d0/a1/d2-d3, -(a0)
            elsif (bytes % 32) >= 12
                movem.l d0/a1/d2, -(a0)
            elsif (bytes % 32) >= 8
                movem.l d0/a1, -(a0)
            elsif (bytes % 32) >= 4
                move.l  d0, -(a0)
            endif
           else
            if bytes / 32 < 256
                moveq   #(bytes / 32) - 1, d1
            else
                movei.w #(bytes / 32) - 1, d1
            endif
.blsfill_loop\?
                movem.l d0/a1/d2-d7, -(a0)
                dbra    d1, .blsfill_loop\?
            if (bytes % 32) >= 28
                movem.l d0/a1/d2-d6, -(a0)
            elsif (bytes % 32) >= 24
                movem.l d0/a1/d2-d5, -(a0)
            elsif (bytes % 32) >= 20
                movem.l d0/a1/d2-d4, -(a0)
            elsif (bytes % 32) >= 16
                movem.l d0/a1/d2-d3, -(a0)
            elsif (bytes % 32) >= 12
                movem.l d0/a1/d2, -(a0)
            elsif (bytes % 32) >= 8
                movem.l d0/a1, -(a0)
            elsif (bytes % 32) >= 4
                move.l  d0, -(a0)
            endif
           endif
                movem.l (sp)+, d2-d7
          endif
        

         endif
        endif
        endif
        endm


; Copy memory from word-aligned addresses.
; dest and src can be anything supported by LEA.
; Addresses must be aligned to a word boundary.
; bytes is the number of bytes to copy. It must be <= 2097152.
; returns address past dest in a1 and past src in a0
BLSFASTCOPY_ALIGNED macro dest, src, bytes
    if bytes == 0
    else

    lea     src, a0                     ; a0 = src : Load source and dest address
    lea     dest, a1                    ; a1 = dst

    if bytes >= $60

    movem.l d2-d7/a2, -(a7)
    ; Compute d0 = block count, a0 = src address, a1 = dest address
    if bytes < ($100*$20)               ; Try to load value into d0 using moveq
    moveq   #(bytes/$20)-1, d0          ; Compute number of $20 bytes blocks
    else
    move.w  #(bytes/$20)-1, d0          ; Use slower move immediate
    endif

    ; Do block copy
.blockcopy_loop\?
    movem.l (a0)+, d1-d7/a2
    movem.l d1-d7/a2, (a1)
    lea     $20(a1), a1
    dbra    d0, .blockcopy_loop\?

    movem.l (a7)+, d2-d7/a2

    ; Copy long data remainder with move.l expansion
    BLSREPEAT2 (bytes & $1F) / 4, move.l (a0)+, (a1)+

    else
    ; Expanded move.l copy
    BLSREPEAT2 bytes / 4, move.l (a0)+, (a1)+
    endif

    BLSREPEAT2 (bytes & 3) / 2, move.w (a0)+, (a1)+
    BLSREPEAT2 (bytes & 1), move.b (a0)+, (a1)+

    endif
    endm

BLS_SETINTVECTOR macro vect, target
    if SCD != 0
    movem.l d0/a0, -(a7)
    ; Put target in d0 and vector in a0
    move.l  #target, d0
    lea     vect, a0
    cmp.l   d0, (a0)        ; Check if vector in ROM already jumps to the correct address
    beq.b   .vector_set\?
    move.l  (a0), a0        ; Point a0 to the target of the interrupt handler
    move.w  #$4EF9, (a0)+   ; Write JMP instruction
    move.l  d0, (a0)+       ; JMP to the target
.vector_set\?
    movem.l (a7)+, d0/a0
    endif
    endm


BLS_ENABLE_INTERRUPTS macro
                andi    #$F8FF, SR
                endm

BLS_DISABLE_INTERRUPTS macro
                ori     #$0700, SR
                endm

ENTER_MONITOR   macro
                trap    #07
                endm

; Delay for a number of CPU cycles
DELAY_CYCLES macro   cycles
    if cycles < 68
        BLSREPEAT cycles / 4, nop
    else
        move    sr, -(sp)       ; 18
        move.w  d0, -(sp)       ; 18
    if cycles >= 72 && cycles < 2632
        moveq   #(cycles - 72) / 10, d0 ; 4
    elsif cycles < 655432
        move.w  #(cycles - 76) / 10, d0 ; 8
    elsif cycles < 2752588
        move.w  #(cycles - 76) / 42, d0
    else
        assert cycles < 2752588 ; Longer loops unimplemented
    endif
    if cycles < 88
        BLSREPEAT (cycles - 72) / 4, nop ; Delay too short to make a loop
    else
.delayloop\?
     if cycles >= 655432
        BLSREPEAT 8, nop        ; Slow down loop for very long waits
     endif
        dbra    d0, .delayloop\?
    endif
        move.w  (sp)+, d0       ; 16
        move    (sp)+, ccr      ; 16
    endif
        endm

; Delay for millis milliseconds
DELAY_MILLIS macro millis
    if millis < 200
     if BUS == BUS_MAIN
        DELAY_CYCLES 7620 * millis
     else
        DELAY_CYCLES 12500 * millis
     endif
    else
        move    sr, -(sp)       ; 18
        move.w  d0, -(sp)       ; 18
        move.w  #(millis - 1), d0 ; 8
    ; First loop is shorter to counter setup time
     if BUS == BUS_MAIN
        DELAY_CYCLES 7620 - 76
     else
        DELAY_CYCLES 12500 - 76
     endif
     if millis > 1
.delaymillisloop\?
      if BUS == BUS_MAIN
        DELAY_CYCLES 7620 - 10
      else
        DELAY_CYCLES 12500 - 10
      endif
        dbra d0, .delaymillisloop\?
        move.w  (sp)+, d0       ; 16
        move    (sp)+, ccr      ; 16
     endif
    endif
        endm

        endif

; vim: ts=8 sw=8 sts=8 et
