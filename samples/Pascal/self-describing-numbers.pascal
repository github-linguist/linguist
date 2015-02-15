Program SelfDescribingNumber;

uses
  SysUtils;

function check(number: longint): boolean;
  var
    i, d: integer;
    a: string;
    count, w : array [0..9] of integer;

  begin
    a := intToStr(number);
    for i := 0 to 9 do
    begin
      count[i] := 0;
      w[i] := 0;
    end;
    for i := 1 to length(a) do
    begin
      d := ord(a[i]) - ord('0');
      inc(count[d]);
      w[i - 1] := d;
    end;
    check := true;
    i := 0;
    while check and (i <= 9) do
    begin
      check := count[i] = w[i];
      inc(i);
    end;
  end;

var
  x: longint;

begin
  writeln ('Autodescriptive numbers from 1 to 100000000:');
  for x := 1 to 100000000 do
    if check(x) then
      writeln (' ', x);
  writeln('Job done.');
end.
