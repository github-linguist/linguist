Program LevenshteinDistanceDemo(output);

uses
  Math;

function LevenshteinDistance(s, t: string): longint;
  var
    d: array of array of integer;
    i, j, n, m: integer;
  begin
    n := length(t);
    m := length(s);
    setlength(d, n+1, m+1);

    for i := 0 to n do
      d[i,0] := i;
    for j := 0 to m do
      d[0,j] := j;
    for j := 1 to n do
      for i := 1 to m do
        if s[i] = t[j] then
          d[i,j] := d[i-1,j-1]
        else
          d[i,j] := min(d[i-1,j] + 1, min(d[i,j-1] + 1, d[i-1,j-1] + 1));
    LevenshteinDistance := d[m,n];
  end;

var
  s1, s2: string;

begin
  s1 := 'kitten';
  s2 := 'sitting';
  writeln('The Levenshtein distance between "', s1, '" and "', s2, '" is: ', LevenshteinDistance(s1, s2));
  s1 := 'rosettacode';
  s2 := 'raisethysword';
  writeln('The Levenshtein distance between "', s1, '" and "', s2, '" is: ', LevenshteinDistance(s1, s2));
end.
