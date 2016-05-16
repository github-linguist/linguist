;===========
; toot13.orc
;===========

sr        =         44100
kr        =         4410
ksmps     =         10
nchnls    =         1

          instr 13
iamp      =         ampdb(p4) / 2       ;amp scaled for two sources
ipluckamp =         p6                  ;p6: % of total amplitude, 1=dB amp as in p4
ipluckdur =         p7*p3               ;p7: % of total duration, 1=entire note duration
ipluckoff =         p3 - ipluckdur

ifmamp    =         p8                  ;p8: % of total amplitude, 1=dB amp as in p4
ifmrise   =         p9*p3               ;p9: % of total duration, 1=entire note duration
ifmdec    =         p10*p3              ;p10: % of total duration
ifmoff    =         p3 - (ifmrise + ifmdec)
index     =         p11
ivibdepth =         p12
ivibrate  =         p13
ifrmntamp =         p14                 ;p14: % of total amplitude, 1=dB amp as in p4
ifrmntris =         p15*p3              ;p15: % of total duration, 1=entire note duration
ifrmntdec =         p3 - ifrmntris

kpluck    linseg    ipluckamp, ipluckdur, 0, ipluckoff, 0
apluck1   pluck     iamp, p5, p5, 0, 1
apluck2   pluck     iamp, p5*1.003, p5*1.003, 0, 1
apluck    =         kpluck * (apluck1+apluck2)

kfm       linseg    0, ifmrise, ifmamp, ifmdec, 0, ifmoff, 0
kndx      =         kfm * index
afm1      foscil    iamp, p5, 1, 2, kndx, 1
afm2      foscil    iamp, p5*1.003, 1.003, 2.003, kndx, 1
afm       =         kfm * (afm1+afm2)

kformant linseg     0, ifrmntris, ifrmntamp, ifrmntdec, 0
kvib      oscil     ivibdepth, ivibrate, 1
afrmt1    fof       iamp, p5+kvib, 650, 0, 40, .003,.017,.007,4,1,2,p3
afrmt2    fof       iamp, (p5*1.001)+kvib*.009, 650, 0, 40, .003,.017,.007,10,1,2,p3
aformant  =         kformant * (afrmt1+afrmt2)

          out       apluck + afm + aformant
          endin


