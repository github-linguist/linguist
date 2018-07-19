Program RunLengthEncoding(output);

procedure encode(s: string; var counts: array of integer; var letters: string);
  var
    i, j: integer;
  begin
    j := 0;
    letters := '';
    if length(s) > 0 then
    begin
      j := 1;
      letters := letters + s[1];
      counts[1] := 1;
      for i := 2 to length(s) do
        if s[i] = letters[j] then
          inc(counts[j])
        else
        begin
          inc(j);
          letters := letters + s[i];
          counts[j] := 1;
        end;
    end;
  end;

procedure decode(var s: string; counts: array of integer; letters: string);
  var
    i, j: integer;
  begin
    s := '';
    for i := 1 to length(letters) do
      for j := 1 to counts[i] do
        s := s + letters[i];
  end;

var
  s: string;
  counts: array of integer;
  letters: string;
  i: integer;
begin
  s := 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWW';
  writeln(s);
  setlength(counts, length(s));
  encode(s, counts, letters);
  for i := 1 to length(letters) - 1 do
    write(counts[i], ' * ', letters[i], ', ');
  writeln(counts[length(letters)], ' * ', letters[length(letters)]);
  decode(s, counts, letters);
  writeln(s);
end.
