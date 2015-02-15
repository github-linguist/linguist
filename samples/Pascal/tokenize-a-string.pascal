program TokenizeString;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;
const
  TestString = 'Hello,How,Are,You,Today';
var
  Tokens: TStringList;
  I: Integer;
begin
  // Uses FCL facilities, "harder" algorithm not implemented
  Tokens := TStringList.Create;
  try
    Tokens.Delimiter := ',';
    Tokens.DelimitedText := TestString;
    Tokens.Delimiter := '.'; // For example
    // To standard Output
    WriteLn(Format('Tokenize from: "%s"', [TestString]));
    WriteLn(Format('to:            "%s"',[Tokens.DelimitedText]));
  finally
    Tokens.Free;
  end;
end.
