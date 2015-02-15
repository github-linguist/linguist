Until =: 2 : 'u^:(-.@:v)^:_'
isHarshad =: 0 = ] |~ [: +/ #.inv  NB. BASE isHarshad N
assert 1 0 -: 10 isHarshad&> 42 38
nextHarshad =: (>: Until (10&isHarshad))@:>:
assert 45 -: nextHarshad 42
assert 3 4 5 -: nextHarshad&> 2 3 4
assert 1 2 3 4 5 6 7 8 9 10 12 18 20 21 24 27 30 36 40 42 -: (, nextHarshad@:{:)Until (20 = #) 1
assert 1002 -: nextHarshad 1000


   NB. next Harshad number in base 6.  Input and output are in base 6.
   NB. Verification left to you, gentle programmer.
   nextHarshad_base_6 =: (>: Until (6&isHarshad))@:>:
   6#.inv nextHarshad_base_6 6b23235
2 3 2 5 3
