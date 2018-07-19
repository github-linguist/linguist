SymmetricDifference := function(a, b)
  return Union(Difference(a, b), Difference(b, a));
end;

a := ["John", "Serena", "Bob", "Mary", "Serena"];
b := ["Jim", "Mary", "John", "Jim", "Bob"];
SymmetricDifference(a,b);
[ "Jim", "Serena" ]
