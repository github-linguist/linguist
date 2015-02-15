Program ConvertDemo(output);

uses
  Math, SysUtils;

const
  alphanum = '0123456789abcdefghijklmnopqrstuvwxyz';

function ToDecimal(base: integer; instring: string): integer;
  var
    inlength, i, n: integer;
  begin
    ToDecimal := 0;
    inlength := length(instring);
    for i := 1 to inlength do
    begin
      n := pos(instring[i], alphanum) - 1;
      n := n * base**(inlength-i);
      Todecimal := ToDecimal + n;
    end;
  end;

function ToBase(base, number: integer): string;
  var
    i, rem: integer;
  begin
    ToBase :='                               ';
    for i := 31 downto 1 do
    begin
      if (number < base) then
      begin
        ToBase[i] := alphanum[number+1];
        break;
      end;
      rem := number mod base;
      ToBase[i] := alphanum[rem+1];
      number := number div base;
    end;
    ToBase := trimLeft(ToBase);
  end;

begin
  writeln ('1A: ', ToDecimal(16, '1a'));
  writeln ('26: ', ToBase(16, 26));
end.
