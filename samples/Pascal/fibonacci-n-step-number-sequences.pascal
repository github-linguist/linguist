program FibbonacciN (output);

type
  TintArray = array of integer;
const
  Name: array[2..11] of string = ('Fibonacci:  ',
                                  'Tribonacci: ',
                                  'Tetranacci: ',
                                  'Pentanacci: ',
                                  'Hexanacci:  ',
                                  'Heptanacci: ',
                                  'Octonacci:  ',
                                  'Nonanacci:  ',
                                  'Decanacci:  ',
                                  'Lucas:      '
                                 );
var
  sequence: TintArray;
  j, k: integer;

function CreateFibbo(n: integer): TintArray;
  var
    i: integer;
  begin
    setlength(CreateFibbo, n);
    CreateFibbo[0] := 1;
    CreateFibbo[1] := 1;
    i := 2;
    while i < n do
    begin
      CreateFibbo[i] := CreateFibbo[i-1] * 2;
      inc(i);
    end;
  end;

procedure Fibbonacci(var start: TintArray);
  const
    No_of_examples = 11;
  var
    n, i, j: integer;
  begin
    n := length(start);
    setlength(start, No_of_examples);
    for i := n to high(start) do
    begin
      start[i] := 0;
      for j := 1 to n do
        start[i] := start[i] + start[i-j]
    end;
  end;

begin
  for j := 2 to 10 do
  begin
    sequence := CreateFibbo(j);
    Fibbonacci(sequence);
    write (Name[j]);
    for k := low(sequence) to high(sequence) do
      write(sequence[k], ' ');
    writeln;
  end;
  setlength(sequence, 2);
  sequence[0] := 2;
  sequence[1] := 1;
  Fibbonacci(sequence);
  write (Name[11]);
  for k := low(sequence) to high(sequence) do
    write(sequence[k], ' ');
  writeln;
end.
