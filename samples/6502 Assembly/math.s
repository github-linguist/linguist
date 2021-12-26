; This file implements various 16-bit math operations.
;
; LICENSE: This program is public domain, and you may do anything and
; everything with it.

; External API
.export math_store_to_arg0
.export math_store_to_arg1
.export math_load_from_mult
.export math_load_from_div
.export math_negate
.export math_abs
.export math_cmp_with_mult
.export math_double
.export math_increase
.export math_multiply
.export math_multiply_add
.export math_divide


; This section contains variables that are uninitialized at start.
.bss
math_arg0     : .res 3   ; 16.8 bit representation
math_arg1     : .res 3   ; 16.8 bit representation
math_res_mult : .res 3   ; 16.8 bit representation
math_res_div  : .res 3   ; 16.8 bit representation
math_tmp      : .res 1   ; Temporary

.code

;
; Store A:X.Y into ARG0
;
math_store_to_arg0:
         sty math_arg0
         stx math_arg0+1
         sta math_arg0+2
         rts


;
; Store A:X.Y into ARG1
;
math_store_to_arg1:
         sty math_arg1
         stx math_arg1+1
         sta math_arg1+2
         rts


;
; Load A:X:Y from MULT
;
math_load_from_mult:
         ldy math_res_mult
         ldx math_res_mult+1
         lda math_res_mult+2
         rts


;
; Load A:X.Y from DIV
;
math_load_from_div:
         ldy math_res_div
         ldx math_res_div+1
         bmi :+                        ; Sign extend to MSB.
         lda #$00
         rts
:        lda #$ff         
         rts


;
; Change sign of A:X.Y
;
math_negate:
         pha
         clc
         tya
         eor #$ff
         adc #$01
         tay
         txa
         eor #$ff
         adc #$00
         tax
         pla
         eor #$ff
         adc #$00
         rts


;
; Calculate the absolute value of A:X.Y
;
math_abs:
         eor #0
         bmi math_negate
         rts


;
; Compare A:X:Y with MULT
;
math_cmp_with_mult:
         cmp math_res_mult+2
         bne :+
         cpx math_res_mult+1
         bne :+
         cpy math_res_mult
:        rts         


;
; Calculate the double of A:X.Y
;
math_double:
         pha
         tya
         asl
         tay
         txa
         rol
         tax
         pla
         rol
         rts


;
; Increase A:X.Y away from 0
;
math_increase:
         cmp #0
         bmi :+                        ; Jump if negative
         pha
         tya
         clc
         adc #1
         tay
         txa
         adc #0
         tax
         pla
         adc #0
         rts
:         
         pha
         tya
         sec
         sbc #1
         tay
         txa
         sbc #0
         tax
         pla
         sbc #0
         rts


;
; Multiply ARG0 with ARG1. Result is in MULT. Destroys both ARG0 and ARG1.
; The multiplication algorithm works by repeated addition of ARG1. The inner
; algorithm requires ARG0 to be positive. Therefore, we must initially check if
; ARG0 is negative, and if so, we negate both ARG0 and ARG1.
; All values are in in 16.8 representation, but the operands ARG0 and ARG1 are
; assume to be within +/- 128.
; Example 1: 0002.00 * 0003.00 = 0006.00
; Example 2: 0001.80 * 0001.80 = 0002.40
;
math_multiply:
         stz math_res_mult
         stz math_res_mult+1
         stz math_res_mult+2

math_multiply_add:
         ; If ARG0 is negative, negate both ARG0 and ARG1
         lda math_arg0+1
         bpl :+                        ; Jump if ARG0 is positive.
         lda math_arg0                 ; Negate ARG0 (ignore MSB)
         eor #$ff
         clc
         adc #$01
         sta math_arg0
         lda math_arg0+1
         eor #$ff
         adc #$00
         sta math_arg0+1
         lda math_arg1                 ; Negate ARG1 (ignore MSB)
         eor #$ff
         clc
         adc #$01
         sta math_arg1
         lda math_arg1+1
         eor #$ff
         adc #$00
         sta math_arg1+1
:
         stz math_tmp
         ldy #$10                      ; We only use 16 bits of ARG0.
@loop:  
         lda math_arg1+1               ; Do an arithmetic shift right, i.e. preserve sign.
         cmp #$80
         ror
         sta math_arg1+1
         ror math_arg1
         ror math_tmp

         bit math_arg0+1
         bpl :+
         clc
         lda math_tmp
         adc math_res_mult
         sta math_res_mult
         lda math_arg1
         adc math_res_mult+1
         sta math_res_mult+1
         lda math_arg1+1
         adc math_res_mult+2
         sta math_res_mult+2
:
         asl math_arg0
         rol math_arg0+1
         dey
         bne @loop
         rts


; Divide ARG0 by ARG1 and place quotient in DIV.
; All values are in 16.8-bit format.
; All values must be positive.
; Example 1: 0002.00 divided by 0004.00 should give 0000.80 as result.
; Example 2: 0004.00 divided by 0002.00 should give 0002.00 as result.
math_divide:
         ldy #$10                ; 16 bits in result.
                                 ; There is no need to initialize math_res_div,
                                 ; because the shifting in of the result
                                 ; removes any previous values.
         stz math_tmp            ; We need an additional high byte.
@loop:
         asl math_arg0
         rol math_arg0+1
         rol math_arg0+2
         rol math_tmp

         lda math_tmp
         cmp math_arg1+2
         bne :+
         lda math_arg0+2
         cmp math_arg1+1
         bne :+
         lda math_arg0+1
         cmp math_arg1
:
         bcc :+                  ; Shift zero into result.
         sec                     ; Carry is always set here, but I'm including this
                                 ; instruction for clarity.
         lda math_arg0+1
         sbc math_arg1
         sta math_arg0+1
         lda math_arg0+2
         sbc math_arg1+1
         sta math_arg0+2
         lda math_tmp
         sbc math_arg1+2
         sta math_tmp
         sec                     ; Carry is always set here, but I'm including this
                                 ; instruction for clarity.
:
         rol math_res_div
         rol math_res_div+1
         rol math_res_div+2
         dey
         bne @loop
         rts

