Program ObtainYN;

uses
  crt;

var
  key: char;

begin
  write('Your answer? (Y/N): ');
  repeat
    key := readkey;
  until (key in ['Y', 'y', 'N', 'n']);
  writeln;
  writeln ('Your answer was: ', key);
end.
