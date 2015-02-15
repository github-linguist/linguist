Program OneOfNLines (Output);

function one_of_n(n: longint): longint;
  var
    i: longint;
  begin
    one_of_n := 1;
    for i := 2 to n do
      if random < 1.0 / i then
	one_of_n := i;
  end;

function sum(a: array of longint): longint;
  var
    i: integer;
  begin
    sum := 0;
    for i := low(a) to high(a) do
      sum := sum + a[i];
  end;

const
  num_reps = 1000000;
  num_lines_in_file = 10;

var
  lines: array[1..num_reps] of longint;
  i: longint;

begin
  randomize;
  for i := 1 to num_reps do
    lines[i] := 0;
  for i := 1 to num_reps do
    inc(lines[one_of_n(num_lines_in_file)]);
  for i := 1 to num_lines_in_file do
    writeln('Number of times line ', i, ' was selected: ', lines[i]);
  writeln('Total number selected: ', sum(lines));
end.
