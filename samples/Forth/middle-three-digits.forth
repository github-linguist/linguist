: middle3          ( n1 -- a n2)
  abs s>d <# #s #> dup 2/ 0<> over 1 and 0<> and
  if 2/ 1- chars + 3 else drop 0 then
;
