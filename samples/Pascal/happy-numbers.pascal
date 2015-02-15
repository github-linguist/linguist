Program HappyNumbers (output);

uses
  Math;

function find(n: integer; cache: array of integer): boolean;
  var
    i: integer;
  begin
    find := false;
    for i := low(cache) to high(cache) do
      if cache[i] = n then
        find := true;
  end;

function is_happy(n: integer): boolean;
  var
    cache: array of integer;
    sum: integer;
  begin
    setlength(cache, 1);
    repeat
      sum := 0;
      while n > 0 do
      begin
        sum := sum + (n mod 10)**2;
        n := n div 10;
      end;
      if sum = 1 then
      begin
        is_happy := true;
        break;
      end;
      if find(sum, cache) then
      begin
        is_happy := false;
        break;
      end;
      n := sum;
      cache[high(cache)]:= sum;
      setlength(cache, length(cache)+1);
    until false;
  end;

var
  n, count: integer;

begin
  n := 1;
  count := 0;
  while count < 8 do
  begin
    if is_happy(n) then
    begin
      inc(count);
      write(n, ' ');
    end;
    inc(n);
  end;
  writeln;
end.
