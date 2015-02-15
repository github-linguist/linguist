Column@{ReleaseHold[
   Function[expression,
     Row@{HoldForm@InputForm@expression, " = ", Quiet@expression},
     HoldAll] /@
    Hold[1./0., 0./0., Limit[-Log[x], x -> 0], Limit[Log[x], x -> 0],
     Infinity + 1, Infinity + Infinity, 2 Infinity,
     Infinity - Infinity, 0 Infinity, ComplexInfinity + 1,
     ComplexInfinity + ComplexInfinity, 2 ComplexInfinity,
     0 ComplexInfinity, Indeterminate + 1, 0 Indeterminate]]}
