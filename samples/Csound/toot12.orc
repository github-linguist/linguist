;===========
; toot12.orc
;===========

          sr        =         44100
          kr        =         4410
          ksmps     =         10
          nchnls    =         1

                    instr 12
          iseed     =         p8
          iamp      =         ampdb(p4)
          kdirect   =         p5
          imeth     =         p6
          ilforate  =         p7                            ; rate for lfo and random index
          itab      =         2
          itabsize  =         8
             
if (imeth == 1)     igoto     direct
if (imeth == 2)     kgoto     lfo
if (imeth == 3)     kgoto     random

direct:   kpitch    table     kdirect, itab                 ; index f2 via p5
                    kgoto     contin

lfo:      kindex    phasor    ilforate
          kpitch    table     kindex * itabsize, itab
                    kgoto     contin

random:   kindex    randh     int(7), ilforate, iseed
          kpitch    table     abs(kindex), itab

contin:   kamp      linseg    0, p3 * .1, iamp, p3 * .9, 0  ; amp envelope
          asig      oscil     kamp, cpspch(kpitch), 1       ; audio oscillator
                    out       asig
                    endin

