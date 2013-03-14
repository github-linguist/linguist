start1 ; entry label
 set ax=1
 set bx=20
 set cx=2
 set ay=1
 set by=20
 set cy=2
 set sumx=0
 set sqrx=0
 set sumxy=0
 for x=ax:cx:bx do
 . set sumx=sumx+x
 . set sqrx=sqrx+(x*x)
 . for y=ay:cy:by do
 .. set sumxy=sumxy+(x*y)
 .. if (sumxy<100) do
 ... write sumxy,!
