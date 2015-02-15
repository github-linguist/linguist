1e fexp fconstant e
0e facos 2e f* fconstant pi  \ predefined in gforth
fsqrt ( f -- f )
fln ( f -- f )   \ flog for base 10
fexp ( f -- f )
fabs ( f -- f )
floor ( f -- f )  \ round towards -inf
: ceil ( f -- f ) fnegate floor fnegate ; \ not standard, though fround is available
f** ( f e -- f^e )
