Program zigzag;

const
  size = 5;
var
  zzarray: array [1..size, 1..size] of integer;
  element, i, j: integer;
  direction: integer;

begin
  i := 1;
  j := 1;
  direction := 1;
  for element := 1 to size*size do
  begin
    zzarray[i,j] := element;
    i := i + direction;
    j := j - direction;
    if (i = 0) then
    begin
      direction := -direction;
      i := i + 1;
    end;
    if (i = size +1) then
    begin
      direction := -direction;
      i := i - 1;
      j := j + 2;
    end;
    if (j = 0) then
    begin
      direction := -direction;
      j := j + 1;
    end;
    if (j = size + 1) then
    begin
      direction := -direction;
      j := j - 1;
      i := i + 2;
    end;
  end;

  for j := 1 to size do
  begin
    for i := 1 to size do
      write(zzarray[i,j]:3);
    writeln;
  end;
end.
