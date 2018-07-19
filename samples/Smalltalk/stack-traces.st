Object subclass: Container [
   Container class >> outer: a and: b and: c [
     self middle: (a+b) and: (b+c)
   ]
   Container class >> middle: x and: y [
     self inner: (x*y)
   ]
   Container class >> inner: k [
     Smalltalk backtrace
   ]
].

Container outer: 2 and: 3 and: 5.

'Anyway, we continue with it' displayNl.
