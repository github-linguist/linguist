;===========
; toot11.sco
;===========

f1   0    2048 10   1                                                                ; Sine 
f2   0    2048 10   1    0.5  0.3  0.25 0.2  0.167     0.14      0.125     .111      ; Sawtooth
f3   0    2048 10   1    0    0.3  0    0.2  0         0.14      0         .111      ; Square
f4   0    2048 10   1    1    1    1    0.7  0.5       0.3       0.1                 ; Pulse


;    ins  strt dur  amp  freq           ; toot11a.sco
     i11  0    1    90   200            ; The Carry Feature
     i11  1    .    .    300
     i11  2    .    .    400
s
     f0   1
s
;    ins  strt dur  amp  freq      
     i11  0    1    90   200
     i11  1    1    90   300
     i11  2    1    90   400
s
     f0   1
s 
;    ins  strt dur  amp  freq           ; toot11b.sco
     i11  0    1    90   200            ; The "+" Feature
     i .  +    .    .    300
     i .  .    .    .    500
s
     f0   1
s
;    ins  strt dur  amp  freq      
     i11  0    1    90   200
     i11  1    1    90   300
     i11  2    1    90   500
s
f0 1
s
;    ins  strt dur  amp  freq           ; toot11c.sco
     i11  0    1    90   100            ; Carry empty p-fields
     i11  +    2
     i11
s
f0 1
s         
;    ins  strt dur  amp  freq      
     i11  0    1    90   100
     i11  1    2    90   100
     i11  3    2    90   100
s
f0 1
s
;    ins  strt dur  amp  freq           ; toot11d.sco
     i11  0    1    90   200            ; The Ramping Feature
     i .  +    .    <    <
     i .  .    .    <    400
     i .  .    .    <    <
     i .  .    4    70   200
s
f0 1
s
;    ins  strt dur  amp  freq      
     i11  0    1    90   200
     i11  1    1    85   300
     i11  2    1    80   400
     i11  3    1    75   300
     i11  4    4    70   200
s
f0 1
s    
;    ins  strt dur  amp  freq           ; toot11e.sco
     i11  0    .5   90   200            ; The Tempo Statement
     i .  +    .    <    <
     i .  .    .    <    400
     i .  .    .    <    <
     i .  .    2    70   200
s
f0 1
s
     t    0    120
;    ins  strt dur  amp  freq           ; Double-time via Tempo
     i11  0    1    90   200
     i .  +    .    <    <
     i .  .    .    <    400
     i .  .    .    <    <
     i .  .    4    70   200

s
f0 1
s

     t    0 60 4    400  8    60        ; toot11f.sco
;    ins  strt dur  amp  freq           ; Time-warping via Tempo
     i11  0    1    70   200
     i .  +    .    <    500
     i .  .    .    90   800
     i .  .    .    <    500
     i .  .    .    70   100
     i .  .    .    90   1000
     i .  .    .    <    600
     i .  .    .    70   200
     i .  .    8    90   100
s
f0 1
s
;    ins  strt dur  amp  freq           ; toot11g.sco
     i11  0    2    90   100            ; The Section Statement
     f0   4                             ; The f0 Statement
     s
     i11  0    1    90   800
     i .  +    .    .    400
     i .  .    .    .    100
     f0   5
     s
     i11  0    4    90   50
s
f0 1
s
;    ins  strt dur  amp  freq           ; toot11h.sco
     i11  0    1    70   100            ; Sorting a Score
     i .  +    .    <    <
     i .  .    .    <    <
     i .  .    .    90   800
     i .  .    .    <    <
     i .  .    .    <    <
     i .  .    .    70   100
     i .  .    .    90   1000
     i .  .    .    <    <
     i .  .    .    <    <
     i .  .    .    <    <
     i .  .    .    70   <
     i .  .    8    90   50
;    ins  strt dur  amp  frq       atk  rel  vbrt vbdpt     vbdl wvfrm
     i6   0    2    86   9.00      .03  .1   6    5         .4   1
     i6   2    2    86   9.02      .03  .1   6    5         .4   2
     i6   4    2    86   9.04      .03  .1   6    5         .4   3
     i6   6    4    86   9.05      .05  .1   6    5         .4   
