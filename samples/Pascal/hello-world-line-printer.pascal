program testprn;
uses printer;
var i: integer;
    f: text;
begin
  writeln ( 'Test of printer unit' );
  writeln ( 'Writing to lst ...' );
  for i := 1 to 80 do
    writeln ( lst, 'This is line', i, '.' #13 );
  close ( lst );
  writeln ( 'Done.' );
  {$ifdef Unix }
  writeln ( 'Writing to pipe ...' );
  assignlst ( f, '|/usr/bin/lpr −m' );
  rewrite ( f );
  for i:= 1 to 80 do
    writeln ( f, 'This is line ', i, '.'#13 );
  close ( f );
  writeln ( 'Done.' )
  {$endif}
end.
