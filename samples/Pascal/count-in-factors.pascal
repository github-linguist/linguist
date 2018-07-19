program CountInFactors(output);

type
  TdynArray = array of integer;

function factorize(number: integer): TdynArray;
  var
    k: integer;
  begin
    if number = 1 then
    begin
      setlength(factorize, 1);
      factorize[0] := 1
    end
    else
    begin
      k := 2;
      while number > 1 do
      begin
	while number mod k = 0 do
	begin
	  setlength(factorize, length(factorize) + 1);
	  factorize[high(factorize)] := k;
	  number := number div k;
	end;
	inc(k);
      end;
    end
  end;

var
  i, j: integer;
  fac: TdynArray;

begin
  for i := 1 to 22 do
  begin
    write(i, ':  ' );
    fac := factorize(i);
    write(fac[0]);
    for j := 1 to high(fac) do
      write(' * ', fac[j]);
    writeln;
  end;
end.
