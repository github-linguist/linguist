Program FileTruncate;

uses
  SysUtils;

const
  filename = 'test';
  position = 7;

var
  myfile: text;
  line: string;
  counter: integer;

begin
  if not FileExists(filename) then
  begin
    writeln('Error: File does not exist.');
    exit;
  end;

  Assign(myfile, filename);
  Reset(myfile);
  counter := 0;
  Repeat
    if eof(myfile) then
    begin
      writeln('Error: The file "', filename, '" is too short. Cannot read line ', position);
      Close(myfile);
      exit;
    end;
    inc(counter);
    readln(myfile);
  until counter = position - 1;
  readln(myfile, line);
  Close(myfile);
  writeln(line);
end.
