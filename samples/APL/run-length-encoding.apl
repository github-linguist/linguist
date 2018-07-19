    ∇ ret←RLL rll;count
[1]   count←∣2-/((1,(2≠/rll),1)×⍳1+⍴rll)~0
[2]   ret←(⍕count,¨(1,2≠/rll)/rll)~' '
    ∇
