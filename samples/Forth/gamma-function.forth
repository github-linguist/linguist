8 constant (gamma-shift)

: (mortici)                            ( f1 -- f2)
  -1 s>f f+ 1 s>f
  fover 271828183e-8 f* 12 s>f f* f/
  fover 271828183e-8 f/ f+
  fover f** fswap
  628318530e-8 f* fsqrt f*             \ 2*pi
;

: gamma                                ( f1 -- f2)
  fdup f0< >r fdup f0= r> or abort" Gamma less or equal to zero"
  fdup (gamma-shift) s>f f+ (mortici) fswap
  1 s>f (gamma-shift) 0 do fover i s>f f+ f* loop fswap fdrop f/
;
