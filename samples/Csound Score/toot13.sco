;===========
; toot13.sco
;===========

f1   0    8192 10   1                                                                ; Sine 
f2   0    2048 19   0.5    1  270 1                                                  ; Sine quadrant rise

;  pluckamp = p6         -    % of total amplitude, 1=dB amp as specified in p4
;  pluckdur = p7*p3      -    % of total duration, 1=entire duration of note

;  fmamp = p8            -    % of total amplitude, 1=dB amp as specified in p4
;  fmrise = p9*p3        -    % of total duration, 1=entire duration of note
;  fmdec = p10*p3        -    % of total duration
;  index = p11           -    number of significant sidebands: p11 + 2
;  vibdepth = p12
;  vibrate = p13
;  formantamp = p14      -    % of total amplitude, 1=dB amp as specified in p4
;  formantrise = p15*p3  -    % of total duration, 1=entire duration of note

 f0 01
 f0 02
 f0 03
 f0 04
 f0 06
 f0 07
 f0 08
 f0 09
 f0 10
 f0 11
 f0 12
 f0 14
 f0 15
 f0 16
 f0 17
 f0 18
 f0 19
 f0 20
 f0 21
 f0 22
 f0 23
 f0 24
 f0 25
 
;ins st dur amp frq plkmp plkdr fmmp  fmrs   fmdc indx vbdp      vbrt frmp fris
i13  0   5  80  200  .8   .3     .7   .2     .35    8     1      5      3   .5
i13  5   8  80  100  .    .4     .7   .35    .35    7     1      6      3   .7
i13 13  13  80   50  .    .3     .7   .2     .4     6     1      4      3   .6

