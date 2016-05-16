;===========
; toot12.sco
;===========

f1   0    4096 10 1                                         ; Sine
f2   0    8    -2 8.00 8.02 8.04 8.05 8.07 8.09 8.11 9.00   ; cpspch C major scale

; method 1 - direct index of table values
; ins     strt dur  amp  index     method    lforate   rndseed
  i12     0    .5   86   7         1         0         0
  i12     .5   .5   86   6         1         0
  i12     1    .5   86   5         1         0
  i12     1.5  .5   86   4         1         0
  i12     2    .5   86   3         1         0
  i12     2.5  .5   86   2         1         0
  i12     3    .5   86   1         1         0
  i12     3.5  .5   86   0         1         0
  i12     4    .5   86   0         1         0
  i12     4.5  .5   86   2         1         0
  i12     5    .5   86   4         1         0
  i12     5.5  2.5  86   7         1         0
  f0      10
s
; method 2 - lfo index of table values
; ins     strt dur  amp  index     method    lforate   rndseed
  i12     0    2    86   0         2         1         0
  i12     3    2    86   0         2         2
  i12     6    2    86   0         2         4
  i12     9    2    86   0         2         8
  i12     12   2    86   0         2         16
  f0      16
s
; method 3 - random index of table values
; ins     strt dur  amp  index     method    lforate   rndseed
  i12     0    2    86   0         3         2         .1
  i12     3    2    86   0         3         3         .2
  i12     6    2    86   0         3         4         .3
  i12     9    2    86   0         3         7         .4
  i12     12   2    86   0         3         11        .5
  i12     15   2    86   0         3         18        .6
  i12     18   2    86   0         3         29        .7
  i12     21   2    86   0         3         47        .8
  i12     24   2    86   0         3         76        .9
  i12     27   2    86   0         3         123       .9
  i12     30   5    86   0         3         199       .
