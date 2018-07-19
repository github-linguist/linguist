include complex.seq

: ZNEGATE ( r i -- -r -i ) fswap fnegate fswap fnegate ;

zvariable x
zvariable y
1e 1e   x z!
pi 1.2e y z!

x z@ y z@ z+ z.
x z@ y z@ z* z.
1+0i x z@ z/ z.
x z@ znegate z.
