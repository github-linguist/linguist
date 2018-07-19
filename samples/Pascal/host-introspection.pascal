program HostIntrospection(output);
begin
  writeln('Pointer size: ', SizeOf(Pointer), ' byte, i.e. ', SizeOf(Pointer)*8, ' bit.');
{ NtoBE converts from native endianess to big endianess }
  if 23453 = NtoBE(23453) then
    writeln('This host is big endian.')
  else
    writeln('This host is little endian.');
end.
