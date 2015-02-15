program StoogeSortDemo;

type
  TIntArray = array of integer;

procedure stoogeSort(var m: TIntArray; i, j: integer);
  var
    t, temp: integer;
  begin
    if m[j] < m[i] then
    begin
      temp := m[j];
      m[j] := m[i];
      m[i] := temp;
    end;
    if j - i > 1 then
    begin
      t := (j - i + 1) div 3;
      stoogesort(m, i, j-t);
      stoogesort(m, i+t, j);
      stoogesort(m, i, j-t);
    end;
  end;

var
  data: TIntArray;
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
  stoogeSort(data, low(data), high(data));
  writeln('The data after sorting:');
  for i := low(data) to high(data) do
  begin
    write(data[i]:4);
  end;
  writeln;
end.
