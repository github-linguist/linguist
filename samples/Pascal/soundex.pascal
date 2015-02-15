program Soundex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
   SysUtils;

type
  TLang=(en,fr,de);

const
   Examples : array[1..16, 1..2] of string =
     (('Ashcraft', 'A261')
     ,('Ashcroft', 'A261')
     ,('Gauss', 'G200')
     ,('Ghosh', 'G200')
     ,('Hilbert', 'H416')
     ,('Heilbronn', 'H416')
     ,('Lee', 'L000')
     ,('Lloyd', 'L300')
     ,('Moses', 'M220')
     ,('Pfister', 'P236')
     ,('Robert', 'R163')
     ,('Rupert', 'R163')
     ,('Rubin', 'R150')
     ,('Tymczak', 'T522')
     ,('Soundex', 'S532')
     ,('Example', 'E251')
     );

// For Ansi Str
function Soundex(Value: String; Lang: TLang) : String;
const
  // Thx to WP.
  Map: array[TLang, 0..2] of String =(
    // Deals with accented, to improve
    ('abcdefghijklmnopqrstuvwxyz'
    ,'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    ,' 123 12- 22455 12623 1-2 2'),
    ('aàâäbcçdeéèêëfghiîjklmnoöôpqrstuùûüvwxyz' // all chars with accented
    ,'AAAABCCDEEEEEFGHIIJKLMNOOOPQRSTUUUUVWXYZ' // uppercased
    ,' 123 97- 72455 12683 9-8 8'),             // coding
    ('abcdefghijklmnopqrstuvwxyz'
    ,'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    ,' 123 12- 22455 12623 1-2 2')
    );
var
  i: Integer;
  c, cOld: Char;

  function Normalize(const s: string): string;
  var
    c: Char;
    p: Integer;
  begin
    result := '';
    for c in LowerCase(s) do
    begin
      p := Pos(c, Map[Lang,0]);
      // unmapped chars are ignored
      if p > 0 then
        Result := Result + Map[Lang, 1][p];
    end;
  End;

  function GetCode(c: Char): Char;
  begin
    Result := Map[Lang, 2][Ord(c)-Ord('A')+1];
  End;

begin
  Value := Trim(Value);
  if Value = '' then
  begin
    Result := '0000';
    exit;
  end;
  Value := Normalize(Value);
  Result := Value[1];
  cOld := GetCode(Value[1]);
  for i := 2 to length(Value) do
  begin
    c := GetCode(Value[i]);
    if (c <> ' ') and (c <> '-') and (c <> cOld) then
      Result := Result + c;
    if c <> '-' then
      cOld := c;
  end;
  Result := Copy(Result+'0000', 1, 4);
End;

const
  Status : array[boolean] of string = ('KO', 'OK');
var
  Found: String;
  tab: array[1..2] of String;
begin
  WriteLn('Word                : Code   Found   Status');
  for tab in Examples do
  begin
    Found := Soundex(tab[1], en);
    WriteLn(Format('%-20s: %s   %s    %s',[tab[1], tab[2], Found, Status[Found = tab[2]]]))
  end;
  ReadLn;
End.
