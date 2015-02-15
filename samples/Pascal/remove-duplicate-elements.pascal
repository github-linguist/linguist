Program RemoveDuplicates;

const
  iArray: array[1..7] of integer = (1, 2, 2, 3, 4, 5, 5);

var
  rArray: array[1..7] of integer;
  i, pos, last: integer;
  newNumber: boolean;

begin
  rArray[1] := iArray[1];
  last := 1;
  pos := 1;
  while pos < high(iArray) do
  begin
    inc(pos);
    newNumber := true;
    for i := low(rArray) to last do
      if iArray[pos] = rArray[i] then
      begin
        newNumber := false;
	break;
      end;
    if newNumber then
    begin
      inc(last);
      rArray[last] := iArray[pos];
    end;
  end;
  for i := low(rArray) to last do
    writeln (rArray[i]);
end.
