program primes;

function prime(n: integer): boolean;
var
  i: integer; max: real;
begin
  if n = 2 then
    prime := true
  else if (n <= 1) or (n mod 2 = 0) then
    prime := false
  else begin
    prime := true; i := 3; max := sqrt(n);
    while i <= max do begin
      if n mod i = 0 then begin
        prime := false; exit
      end;
      i := i + 2
    end
  end
end;

{ Test and display primes 0 .. 50 }
var
  n: integer;
begin
  for n := 0 to 50 do
    if (prime(n)) then
      write(n, ' ');
end.
