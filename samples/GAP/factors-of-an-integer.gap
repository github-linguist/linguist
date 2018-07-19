# Built-in function
DivisorsInt(Factorial(5));
# [ 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40, 60, 120 ]

# A possible implementation, not suitable to large n
div := n -> Filtered([1 .. n], k -> n mod k = 0);

div(Factorial(5));
# [ 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40, 60, 120 ]

# Another implementation, usable for large n (if n can be factored quickly)
div2 := function(n)
  local f, p;
  f := Collected(FactorsInt(n));
  p := List(f, v -> List([0 .. v[2]], k -> v[1]^k));
  return SortedList(List(Cartesian(p), Product));
end;

div2(Factorial(5));
# [ 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40, 60, 120 ]
