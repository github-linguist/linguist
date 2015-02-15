# Using built-in functions
Incr := s -> String(Int(s) + 1);

# Implementing addition
# (but here 9...9 + 1 = 0...0 since the string length is fixed)
Increment := function(s)
  local c, n, carry, digits;
  digits := "0123456789";
  n := Length(s);
  carry := true;
  while n > 0 and carry do
    c := Position(digits, s[n]) - 1;
    if carry then
      c := c + 1;
    fi;
    if c > 9 then
      carry := true;
      c := c - 10;
    else
      carry := false;
    fi;
    s[n] := digits[c + 1];
    n := n - 1;
  od;
end;

s := "2399";
Increment(s);
s;
# "2400"
