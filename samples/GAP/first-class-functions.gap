# Function composition
Composition := function(f, g)
  local h;
  h := function(x)
    return f(g(x));
  end;
  return h;
end;

# Apply each function in list u, to argument x
ApplyList := function(u, x)
  local i, n, v;
  n := Size(u);
  v := [ ];
  for i in [1 .. n] do
    v[i] := u[i](x);
  od;
  return v;
end;

# Inverse and Sqrt are in the built-in library. Note that GAP doesn't have real numbers nor floating point numbers. Therefore, Sqrt yields values in cyclotomic fields.
# For example,
#    gap> Sqrt(7);
#    E(28)^3-E(28)^11-E(28)^15+E(28)^19-E(28)^23+E(28)^27
# where E(n) is a primitive n-th root of unity
a := [ i -> i + 1, Inverse, Sqrt ];
# [ function( i ) ... end, <Operation "InverseImmutable">, <Operation "Sqrt"> ]
b := [ i -> i - 1, Inverse, x -> x*x ];
# [ function( i ) ... end, <Operation "InverseImmutable">, function( x ) ... end ]

# Compose each couple
z := ListN(a, b, Composition);

# Now a test
ApplyList(z, 3);
[ 3, 3, 3 ]
