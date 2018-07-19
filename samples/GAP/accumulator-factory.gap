accu := function(n)
  local f, v;
  v := n;
  f := function(a)
    v := v + a;
    return v;
  end;
  return f;
end;

a := accu(0);
# function( a ) ... end
b := accu(100);
# function( a ) ... end
a(6);
# 6
b(6);
# 106
a(1);
# 7
b(1);
# 107
# These functions also accept other types, as long as addition is meaningful
b(1/FLOAT_INT(3))
# 107.333
a(2/3);
# 23/3
a([1, 2, 3]);
# [ 26/3, 29/3, 32/3 ]
