Filter=: (#~`)(`:6)
Filter=: (#~`)(`:6)

f=: 3 : 0
BASE=. y
primes=. (1&p:)Filter
DIGITS=. }. i. x: BASE
R=. 0
A=. ,0
while. #A do.
 R=. >: R
 B=. A
 A=. primes , , A +/ BASE #. (_,R) {. ,. DIGITS
end.
>./ B
)
l
NB.   f N  where N is the base.
   (,. f"0) (3+i.8)
 3                       23
 4                     4091
 5                     7817
 6            4836525320399
 7                   817337
 8           14005650767869
 9               1676456897
10 357686312646216567629137

   f 11
2276005673
