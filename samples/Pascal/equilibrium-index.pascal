Program EquilibriumIndexDemo(output);

function ArraySum(list: array of integer; first, last: integer): integer;
  var
    i: integer;
  begin
    ArraySum := 0;
    for i := first to last do  // not taken if first > last
      ArraySum := ArraySum + list[i];
  end;

procedure EquilibriumIndex(list: array of integer; offset: integer);
  var
    i: integer;
  begin
    for i := low(list) to high(list) do
      if ArraySum(list, low(list), i-1) = ArraySum(list, i+1, high(list)) then
        write(offset + i:3);
  end;

var
{** The base index of the array is fully taken care off and can be any number. **}
  numbers: array [1..7] of integer = (-7, 1, 5, 2, -4, 3, 0);
  i: integer;

begin
  write('List of numbers: ');
  for i := low(numbers) to high(numbers) do
    write(numbers[i]:3);
  writeln;
  write('Equilibirum indices: ');
  EquilibriumIndex(numbers, low(numbers));
  writeln;
end.
