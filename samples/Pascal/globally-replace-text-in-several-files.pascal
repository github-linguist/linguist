Program StringReplace;

uses
  Classes, StrUtils;

const
  fileName: array[1..3] of string = ('a.txt', 'b.txt', 'c.txt');
  matchText = 'Goodbye London!';
  replaceText = 'Hello New York!';

var
  AllText: TStringlist;
  i, j: integer;

begin
  for j := low(fileName) to high(fileName) do
  begin
   AllText := TStringlist.Create;
   AllText.LoadFromFile(fileName[j]);
   for i := 0 to AllText.Count-1 do
     AllText.Strings[i] := AnsiReplaceStr(AllText.Strings[i], matchText, replaceText);
   AllText.SaveToFile(fileName[j]);
   AllText.Destroy;
  end;
end.
