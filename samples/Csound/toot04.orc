;===========
; toot04.orc 
;===========

sr        =         44100
kr        =         4410
ksmps     =         10
nchnls    =         1

          instr 4
iamp      =         ampdb(p4)           ; convert decibels to linear amp
iscale    =         iamp * .333         ; scale the amp at initialization
inote     =         cpspch(p5)          ; convert octave.pitch to cps

k1        linen     iscale, p6, p3, p7  ; p4=amp

a3        oscil     k1, inote*.996, 1   ; p5=freq
a2        oscil     k1, inote*1.004, 1  ; p6=attack time
a1        oscil     k1, inote, 1        ; p7=release time

a1        =         a1+a2+a3
          out       a1          
          endin


