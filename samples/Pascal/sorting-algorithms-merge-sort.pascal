program MergeSortDemo;

type
  TIntArray = array of integer;

function merge(left, right: TIntArray): TIntArray;
  var
    i, j: integer;
  begin
    j := 0;
    setlength(merge, length(left) + length(right));
    while (length(left) > 0) and (length(right) > 0) do
    begin
      if left[0] <= right[0] then
      begin
	merge[j] := left[0];
	inc(j);
	for i := low(left) to high(left) - 1 do
	  left[i] := left[i+1];
	setlength(left, length(left) - 1);
      end
      else
      begin
	merge[j] := right[0];
	inc(j);
	for i := low(right) to high(right) - 1 do
	  right[i] := right[i+1];
	setlength(right, length(right) - 1);
      end;
    end;
    if length(left) > 0 then
      for i := low(left) to high(left) do
	  merge[j + i] := left[i];
    j := j + length(left);
    if length(right) > 0 then
      for i := low(right) to high(right) do
	  merge[j + i] := right[i];
  end;

function mergeSort(m: TIntArray): TIntArray;
  var
    left, right: TIntArray;
    i, middle: integer;
  begin
    setlength(mergeSort, length(m));
    if length(m) = 1 then
      mergeSort[0] := m[0]
    else if length(m) > 1 then
    begin
      middle := length(m) div 2;
      setlength(left, middle);
      setlength(right, length(m)-middle);
      for i := low(left) to high(left) do
        left[i] := m[i];
      for i := low(right) to high(right) do
        right[i] := m[middle+i];
      left  := mergeSort(left);
      right := mergeSort(right);
      mergeSort := merge(left, right);
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
  data := mergeSort(data);
  writeln('The data after sorting:');
  for i := low(data) to high(data) do
  begin
    write(data[i]:4);
  end;
  writeln;
end.
