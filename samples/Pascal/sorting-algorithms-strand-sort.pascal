program StrandSortDemo;

type
  TIntArray = array of integer;

function merge(left: TIntArray; right: TIntArray): TIntArray;
  var
    i, j, k: integer;
  begin
    setlength(merge, length(left) + length(right));
    i := low(merge);
    j := low(left);
    k := low(right);
    repeat
      if ((left[j] <= right[k]) and (j <= high(left))) or (k > high(right)) then
      begin
        merge[i] := left[j];
        inc(j);
      end
      else
      begin
        merge[i] := right[k];
        inc(k);
      end;
      inc(i);
    until i > high(merge);
  end;

function StrandSort(s: TIntArray): TIntArray;
  var
    strand: TIntArray;
    i, j: integer;
  begin
    setlength(StrandSort, length(s));
    setlength(strand, length(s));
    i := low(s);
    repeat
      StrandSort[i] := s[i];
      inc(i);
    until (s[i] < s[i-1]);
    setlength(StrandSort, i);
    repeat
      setlength(strand, 1);
      j := low(strand);
      strand[j] := s[i];
      while (s[i+1] > s[i]) and (i < high(s)) do
      begin
        inc(i);
        inc(j);
	setlength(strand, length(strand) + 1);
        Strand[j] := s[i];
      end;
      StrandSort := merge(StrandSort, strand);
      inc(i);
    until (i > high(s));
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
  data := StrandSort(data);
  writeln('The data after sorting:');
  for i := low(data) to high(data) do
  begin
    write(data[i]:4);
  end;
  writeln;
end.
