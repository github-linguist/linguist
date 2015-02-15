MovingAverage := function(n)
  local sma, buffer, pos, sum, len;
  buffer := List([1 .. n], i -> 0);
  pos := 0;
  len := 0;
  sum := 0;
  sma := function(x)
    pos := RemInt(pos, n) + 1;
    sum := sum + x - buffer[pos];
    buffer[pos] := x;
    len := Minimum(len + 1, n);
    return sum/len;
  end;
  return sma;
end;

f := MovingAverage(3);
f(1);  #  1
f(2);  #  3/2
f(3);  #  2
f(4);  #  3
f(5);  #  4
f(5);  #  14/3
f(4);  #  14/3
f(3);  #  4
f(2);  #  3
f(1);  #  2
