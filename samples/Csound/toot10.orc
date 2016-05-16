;===========
; toot10.orc
;===========

sr        =         44100
kr        =         4410
ksmps     =         10
nchnls    =         2

garvbsig init       0

          instr 10
iattack   =         .01
irelease  =         .2
iwhite    =         22050
idur      =         p3
iamp      =         p4
iswpstart =         p5
isweepend =         p6
ibndwidth =         p7
ibalance  =         p8                  ; 1 = left, .5 = center, 0 = right
irvbgain  =         p9
kamp      linen     iamp, iattack, idur, irelease
ksweep    line      iswpstart, idur, isweepend
asig      rand      iwhite
afilt     reson     asig, ksweep, ibndwidth
arampsig  =         kamp * afilt
          outs      arampsig * ibalance, arampsig * (1 - ibalance)
garvbsig  =         garvbsig + arampsig * p9
          endin

          instr     100
irvbtime  =         p4
asig      reverb    garvbsig,  irvbtime
          outs      asig, asig
garvbsig  =         0
          endin



