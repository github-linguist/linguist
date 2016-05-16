;===========
; toot08.orc
;===========

sr        =         44100
kr        =         4410
ksmps     =         10
nchnls    =         1

          instr 8
idur      =         p3
iamp      =         p4
iskiptime =         p5
iattack   =         p6
irelease  =         p7
irvbtime  =         p8
irvbgain  =         p9
                                      
kamp      linen     iamp, iattack, idur, irelease
asig      soundin   "hellorcb.aif", iskiptime
arampsig  =         kamp * asig
aeffect   reverb    asig, irvbtime
arvbretrn =         aeffect * irvbgain
          out       arampsig + arvbretrn
          endin

