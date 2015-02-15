program StripCharacters(output);

function Strip (s: string; control, extended: boolean): string;
  var
    index: integer;
  begin
    Strip := '';
    for index:= 1 to length(s) do
    if not ((control and (ord(s[index]) <= 32)) or (extended and (ord(s[index]) > 127))) then
      Strip := Strip + s[index];
  end;

var
  test: string;
  i: integer;

begin
  setlength(test, 40);
  randomize;
  for i := 1 to length(test) do
    test[i] := char(1 + random(255));
  writeln ('Original: ', test);
  writeln ('No CNTL:  ', Strip(test, true,  false));
  writeln ('No extnd: ', Strip(test, false, true));
  writeln ('ASCII:    ', Strip(test, true,  true));
end.
