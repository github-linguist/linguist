Program BoundInteger(output);

{$RANGECHECKS ON}

type
  TPartialInteger = 1..10;

var
  testvar: TPartialInteger;
  i: integer;

begin
  for i := 1 to 11 do
  begin
    writeln(i);
    testvar := i;
  end;
end.
