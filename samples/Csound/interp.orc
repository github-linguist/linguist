sr     = 44100
kr     = 44100
ksmps  = 1
nchnls = 2

; pvanal -n 1024 -w 2 partA-L.wav partA-L.pvc
; pvanal -n 1024 -w 2 partA-R.wav partA-R.pvc
; pvanal -n 1024 -w 2 partB.wav partB.pvc
instr 1
  iscale = 1

  ktimpnt1     line 0, iscale*(82196/44100), 82196/44100
  ktimpnt2     linseg 0, iscale*1.25, 0, iscale*(103518/44100), 103518/44100
  kfreqscale   linseg 1, iscale*0.5, 1, iscale*1.6, 0.8
  kfreqinterpL linseg 0, iscale*0.25, 0, iscale*1.6, 1
  kampinterpL  linseg 0, iscale*0.25, 0, iscale*1.6, 1
  kfreqinterpR linseg 0, iscale*0.5, 0, iscale*1.2, 1
  kampinterpR  linseg 0, iscale*0.5, 0, iscale*1.2, 1

        pvbufread ktimpnt1, "partB.pvc"
  apvcL pvinterp ktimpnt2, 1, "partA-L.pvc", kfreqscale, 1, 1, 1, 1-kfreqinterpL, 1-kampinterpL
        pvbufread ktimpnt1, "partB.pvc"
  apvcR pvinterp ktimpnt2, 1, "partA-R.pvc", kfreqscale, 1, 1, 1, 1-kfreqinterpR, 1-kampinterpR

  outs apvcL*0.8, apvcR*0.8
endin
