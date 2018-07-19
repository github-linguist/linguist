Program AveragesMedian(output);

type
  TDoubleArray = array of double;

procedure bubbleSort(var list: TDoubleArray);
var
  i, j, n: integer;
  t: double;
begin
  n := length(list);
  for i := n downto 2 do
    for j := 0 to i - 1 do
      if list[j] > list[j + 1] then
      begin
        t := list[j];
        list[j] := list[j + 1];
        list[j + 1] := t;
      end;
end;

function Median(aArray: TDoubleArray): double;
var
  lMiddleIndex: integer;
begin
  bubbleSort(aArray);
  lMiddleIndex := (high(aArray) - low(aArray)) div 2;
  if Odd(Length(aArray)) then
    Median := aArray[lMiddleIndex + 1]
  else
    Median := (aArray[lMiddleIndex + 1] + aArray[lMiddleIndex]) / 2;
end;

var
  A: TDoubleArray;
  i: integer;

begin
  randomize;
  setlength(A, 7);
  for i := low(A) to high(A) do
  begin
    A[i] := 100 * random;
    write (A[i]:7:3, ' ');
  end;
  writeln;
  writeln('Median: ', Median(A):7:3);

  setlength(A, 6);
  for i := low(A) to high(A) do
  begin
    A[i] := 100 * random;
    write (A[i]:7:3, ' ');
  end;
  writeln;
  writeln('Median: ', Median(A):7:3);
end.
