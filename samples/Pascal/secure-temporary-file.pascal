Program TempFileDemo;

uses
  SysUtils;

var
  tempFile: text;

begin
  assign (Tempfile, GetTempFileName);
  rewrite (tempFile);
  writeln (tempFile, 5);
  close (tempFile);
end.
