 arithmetic←{(+/⍵)÷⍴⍵}
 geometric←{(×/⍵)*÷⍴⍵}
 harmonic←{(⍴⍵)÷(+/÷⍵)}


 x←⍳10

 arithmetic x
5.5
 geometric x
4.528728688
 harmonic x
3.414171521
