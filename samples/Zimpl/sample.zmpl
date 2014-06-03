# $Id: queens3.zpl,v 1.3 2009/09/13 16:15:53 bzfkocht Exp $
#
# This is a formulation of the n queens problem using binary variables.
# variables. Since the number of queens is maximized, the size of the
# board can be set arbitrarily.
#
param columns := 8;

set I   := { 1 .. columns };
set IxI := I * I;

set TABU[<i,j> in IxI] := { <m,n> in IxI with 
   (m != i or n != j) and (m == i or n == j or abs(m - i) == abs(n - j)) };

var x[IxI] binary;

maximize queens: sum <i,j> in IxI : x[i,j];

subto c1: forall <i,j> in IxI do
   card(TABU[i,j]) - card(TABU[i,j]) * x[i,j] >= sum <m,n> in TABU[i,j] : x[m,n];
   
