x := Indeterminate(Rationals, "x");
p := x^11 + 3*x^8 + 7*x^2 + 3;
q := x^7 + 5*x^3 + 1;
QuotientRemainder(p, q);
# [ x^4+3*x-5, -16*x^4+25*x^3+7*x^2-3*x+8 ]
