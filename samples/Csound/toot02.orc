;===========
; toot02.orc
;===========

sr        =         44100
kr        =         4410
ksmps     =         10
nchnls    =         1

          instr 2
a1        oscil     p4, p5, 1      ; p4=amp
          out       a1             ; p5=freq
          endin





