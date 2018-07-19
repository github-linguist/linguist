Program PrimeDecomposition(output);

type
  DynArray = array of integer;

procedure findFactors(n: Int64; var d: DynArray);
  var
    divisor, next, rest: Int64;
    i: integer;
 begin
    i := 0;
    divisor := 2;
    next := 3;
    rest := n;
    while (rest <> 1) do
    begin
      while (rest mod divisor = 0) do
      begin
        setlength(d, i+1);
        d[i] := divisor;
        inc(i);
        rest := rest div divisor;
      end;
      divisor := next;
      next := next + 2;
    end;
  end;

var
  factors: DynArray;
  j: integer;

begin
  setlength(factors, 1);
  findFactors(1023*1024, factors);
  for j := low(factors) to high(factors) do
    writeln (factors[j]);
end.
