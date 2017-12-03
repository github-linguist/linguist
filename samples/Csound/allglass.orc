sr     = 44100
kr     = 44100
ksmps  = 1
nchnls = 2

; pvanal -n 512 -w 8 allglass1-L.wav allglass1-L.pvc
; pvanal -n 512 -w 8 allglass1-R.wav allglass1-R.pvc
instr 1
  ktime line 0, p3, 17.5018
  arL pvoc ktime, 1, "allglass1-L.pvc"
  arR pvoc ktime, 1, "allglass1-R.pvc"
  out arL, arR
endin
