;===========
; toot11.orc
;===========

sr        =         44100
kr        =         4410
ksmps     =         10
nchnls    =         1

          instr 6
ifunc     =         p11                                ; select the basic waveform
irel      =         0.01                               ; set vibrato release time
idel1     =         p3 - (p10 * p3)                    ; calculate initial delay (% of dur)
isus      =         p3 - (idel1- irel)                 ; calculate remaining duration             
iamp      =         ampdb(p4)
iscale    =         iamp * .333                        ; p4=amp
inote     =         cpspch(p5)                         ; p5=freq             
k3        linseg    0, idel1, p9, isus, p9, irel, 0    ; p6=attack time
k2        oscil     k3, p8, 1                          ; p7=release time
k1        linen     iscale, p6, p3, p7                 ; p8=vib rate
a3        oscil     k1, inote*.999+k2, ifunc           ; p9=vib depth
a2        oscil     k1, inote*1.001+k2, ifunc          ; p10=vib delay (0-1)
a1        oscil     k1, inote+k2, ifunc
          out       a1+a2+a3
          endin

		instr 11
asig1	pluck	ampdb(p4)/2, p5, p5, 0, 1
asig2	pluck	ampdb(p4)/2, p5*1.003, p5*1.003, 0, 1
		out		asig1 + asig2
		endin


