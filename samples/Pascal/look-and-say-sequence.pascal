program LookAndSayDemo(input, output);

uses
  SysUtils;

function LookAndSay (s: string): string;
  var
    item: char;
    index: integer;
    count: integer;
  begin
    LookAndSay := '';
    item := s[1];
    count := 1;
    for index:= 2 to length(s) do
      if item = s[index] then
        inc(count)
      else
      begin
	LookAndSay := LookAndSay + intTostr(count) + item;
        item := s[index];
	count := 1;
      end;
      LookAndSay := LookAndSay + intTostr(count) + item;
  end;

var
  number: string;

begin
  writeln('Press RETURN to continue and ^C to stop.');
  number := '1';
  while not eof(input) do
  begin
   write(number);
   readln;
   number := LookAndSay(number);
  end;
end.
