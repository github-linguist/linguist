Program PickRandomElement (output);

const
  s: array [1..5] of string = ('1234', 'ABCDE', 'Charlie', 'XB56ds', 'lala');

begin
  randomize;
  writeln(s[low(s) + random(length(s))]);
end.
