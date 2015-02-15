      zz   ←  {⍵⍴⎕IO-⍨⍋⊃,/{(2|⍴⍵):⌽⍵⋄⍵}¨(⊂w)/¨⍨w{↓⍵∘.=⍨∪⍵}+/[1]⍵⊤w←⎕IO-⍨⍳×/⍵}   ⍝  General zigzag (any rectangle)
      zzSq ←  {zz,⍨⍵}                                                           ⍝  Square zigzag
      zzSq 5
 0  1  5  6 14
 2  4  7 13 15
 3  8 12 16 21
 9 11 17 20 22
10 18 19 23 24
