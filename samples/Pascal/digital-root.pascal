program DigitalRoot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, StrUtils;

// FPC has no Big mumbers implementation, Int64 will suffice.

procedure GetDigitalRoot(Value: Int64; Base: Byte; var DRoot, Pers: Integer);
var
  i: Integer;
  DigitSum: Int64;
begin
  Pers := 0;
  repeat
    Inc(Pers);
    DigitSum := 0;
    while Value > 0 do
    begin
      Inc(DigitSum, Value mod Base);
      Value := Value div Base;
    end;
    Value := DigitSum;
  until Value < Base;
  DRoot := Value;
End;

function IntToStrBase(Value: Int64; Base: Byte):String;
const
  // usable up to 36-Base
  DigitSymbols = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXY';
begin
  Result := '';
  while Value > 0 do
  begin
    Result := DigitSymbols[Value mod Base+1] + Result;
    Value := Value div Base;
  End;

End;

procedure Display(const Value: Int64; Base: Byte = 10);
var
  DRoot, Pers: Integer;
  StrValue: string;
begin
  GetDigitalRoot(Value, Base, DRoot, Pers);
  WriteLn(Format('%s(%d) has additive persistence %d and digital root %d.',
    [IntToStrBase(Value, Base), Base, Pers, DRoot]));
End;

begin
  WriteLn('--- Examples in 10-Base ---');
  Display(627615);
  Display(39390);
  Display(588225);
  Display(393900588225);

  WriteLn('--- Examples in 16-Base ---');
  Display(627615, 16);
  Display(39390, 16);
  Display(588225, 16);
  Display(393900588225, 16);

  ReadLn;
End.
