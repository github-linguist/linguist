program BestShuffleDemo(output);

function BestShuffle(s: string): string;

  var
    tmp: char;
    i, j: integer;
    t: string;
  begin
    t := s;
    for i := 1 to length(t) do
      for j := 1 to length(t) do
        if (i <> j) and (s[i] <> t[j]) and (s[j] <> t[i]) then
        begin
          tmp  := t[i];
          t[i] := t[j];
          t[j] := tmp;
        end;
    BestShuffle := t;
  end;

const
  original: array[1..6] of string =
    ('abracadabra', 'seesaw', 'elk', 'grrrrrr', 'up', 'a');

var
  shuffle: string;
  i, j, score: integer;

begin
 for i := low(original) to high(original) do
 begin
   shuffle := BestShuffle(original[i]);
   score := 0;
   for j := 1 to length(shuffle) do
     if original[i][j] = shuffle[j] then
       inc(score);
    writeln(original[i], ', ', shuffle, ', (', score, ')');
  end;
end.
