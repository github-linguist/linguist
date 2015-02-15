Program FileTruncate;

uses
  SysUtils;

var
  myfile:   file of byte;
  filename: string;
  position: integer;

begin
  write('File for truncation: ');
  readln(filename);
  if not FileExists(filename) then
  begin
    writeln('Error: File does not exist.');
    exit;
  end;

  write('Truncate position: ');
  readln(position);

  Assign(myfile, filename);
  Reset(myfile);
  if FileSize(myfile) < position then
  begin
    writeln('Warning: The file "', filename, '" is too short. No need to truncate at position ', position);
    Close(myfile);
    exit;
  end;

  Seek(myfile, position);
  Truncate(myfile);
  Close(myfile);
  writeln('File "', filename, '" truncated at position ', position, '.');
end.
