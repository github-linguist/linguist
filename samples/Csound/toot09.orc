;===========
; toot09.orc
;===========

sr        =         44100
kr        =         4410
ksmps     =         10
nchnls    =         2                        ; stereo output

garvbsig  init      0                        ; global "a" variable initialized to 0

          instr 9
idur      =         p3
iamp      =         p4
iskiptime =         p5
iattack   =         p6
irelease  =         p7
ibalance  =         p8                       ; 1 = left, .5 = center, 0 = right
irvbgain  =         p9

kamp      linen     iamp, iattack, idur, irelease
asig      soundin   "hellorcb.aif", iskiptime
arampsig  =         kamp * asig
          outs      arampsig * ibalance, arampsig * (1 - ibalance)
garvbsig  =         garvbsig + arampsig * irvbgain
          endin


          instr 99
irvbtime  =         p4
asig      reverb    garvbsig,  irvbtime      ; put global sig into reverb
          outs      asig, asig
garvbsig  =         0                        ; then clear it
          endin

