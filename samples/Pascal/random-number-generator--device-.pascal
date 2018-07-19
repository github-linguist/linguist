program RandomNumberDevice;
var
  byteFile: file of byte;
  randomByte: byte;
begin
  assign(byteFile, '/dev/urandom');
  reset (byteFile);
  read  (byteFile, randomByte);
  close (byteFile);
  writeln('The random byte is: ', randomByte);
end.
