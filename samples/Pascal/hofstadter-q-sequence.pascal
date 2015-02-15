Program HofstadterQSequence (output);

const
  limit = 100000;

var
  q: array [1..limit] of longint;
  i, flips: longint;

begin
  q[1] := 1;
  q[2] := 1;
  for i := 3 to limit do
    q[i] := q[i - q[i - 1]] + q[i - q[i - 2]];
  for i := 1 to 10 do
    write(q[i], ' ');
  writeln;
  writeln(q[1000]);
  flips := 0;
  for i := 1 to limit - 1 do
    if q[i] > q[i+1] then
      inc(flips);
  writeln('Flips: ', flips);
end.
