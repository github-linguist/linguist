Program ExponentiationOperator(output);

function intexp (base, exponent: integer): longint;
  var
    i: integer;

  begin
    if (exponent < 0) then
      if (base = 1) then
        intexp := 1
      else
        intexp := 0
    else
    begin
      intexp := 1;
      for i := 1 to exponent do
        intexp := intexp * base;
    end;
  end;

function realexp (base: real; exponent: integer): real;
  var
    i: integer;

  begin
    realexp := 1.0;
    if (exponent < 0) then
      for i := exponent to -1 do
        realexp := realexp / base
    else
      for i := 1 to exponent do
        realexp := realexp * base;
  end;

begin
  writeln('2^30: ', intexp(2, 30));
  writeln('2.0^30: ', realexp(2.0, 30));
end.
