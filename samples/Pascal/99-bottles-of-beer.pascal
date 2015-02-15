procedure BottlesOfBeer;
var
  i: Integer;
begin
  for i := 99 downto 1 do
  begin
    if i = 1 then
    begin
      WriteLn('1 bottle of beer on the wall');
      WriteLn('1 bottle of beer');
      WriteLn('Take one down, pass it around');
      WriteLn('No more bottles of beer on the wall');
      Exit;
    end;
    WriteLn(Format('%d bottles of beer on the wall', [i]));
    WriteLn(Format('%d bottles of beer', [i]));
    WriteLn('Take one down, pass it around');
    WriteLn(Format('%d bottles of beer on the wall', [Pred(i)]));
    WriteLn('');
  end;
end;
