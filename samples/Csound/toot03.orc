;===========
; toot03.orc 
;===========

sr        =         44100
kr        =         4410
ksmps     =         10
nchnls    =         1

          instr 3                       ; p3=duration of note
k1        linen     p4, p6, p3, p7      ; p4=amp
a1        oscil     k1, p5, 1           ; p5=freq
          out       a1                  ; p6=attack time
          endin                         ; p7=release time


