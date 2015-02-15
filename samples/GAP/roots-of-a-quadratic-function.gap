QuadraticRoots := function(a, b, c)
  local d;
  d := Sqrt(b*b - 4*a*c);
  return [ (-b+d)/(2*a), (-b-d)/(2*a) ];
end;

# Hint : E(12) is a 12th primitive root of 1
QuadraticRoots(2, 2, -1);
# [ 1/2*E(12)^4-1/2*E(12)^7+1/2*E(12)^8+1/2*E(12)^11,
#   1/2*E(12)^4+1/2*E(12)^7+1/2*E(12)^8-1/2*E(12)^11 ]

# This works also with floating-point numbers
QuadraticRoots(2.0, 2.0, -1.0);
# [ 0.366025, -1.36603 ]
