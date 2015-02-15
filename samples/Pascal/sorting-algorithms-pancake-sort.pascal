Program PancakeSort (output);

procedure flip(var b: array of integer; last: integer);

  var
    swap, i: integer;

  begin
    for i := low(b) to (last - low(b) - 1) div 2 do
    begin
      swap              := b[i];
      b[i]              := b[last-(i-low(b))];
      b[last-(i-low(b))] := swap;
    end;
  end;

procedure PancakeSort(var a: array of integer);

  var
    i, j, maxpos: integer;

  begin
    for i := high(a) downto low(a) do
    begin
// Find position of max number between beginning and i
      maxpos := i;
      for j := low(a) to i - 1 do
        if a[j] > a[maxpos] then
          maxpos := j;

// is it in the correct position already?
      if maxpos = i then
        continue;

// is it at the beginning of the array? If not flip array section so it is
      if maxpos <> low(a) then
        flip(a, maxpos);

// Flip array section to get max number to correct position
      flip(a, i);
    end;
  end;

var
  data: array of integer;
  i: integer;

begin
  setlength(data, 8);
  Randomize;
  writeln('The data before sorting:');
  for i := low(data) to high(data) do
  begin
    data[i] := Random(high(data));
    write(data[i]:4);
  end;
  writeln;
  PancakeSort(data);
  writeln('The data after sorting:');
  for i := low(data) to high(data) do
  begin
    write(data[i]:4);
  end;
  writeln;
end.
