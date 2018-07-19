program PerfectNumbers;

 function isPerfect(number: longint): boolean;
 var
  i, sum: longint;

 begin
  sum := 1;
  for i := 2 to round(sqrt(real(number))) do
    if (number mod i = 0) then
     sum := sum + i + (number div i);
  isPerfect := (sum = number);
 end;

var
 candidate: longint;

begin
 writeln('Perfect numbers from 1 to 33550337:');
 for candidate := 2 to 33550337 do
   if isPerfect(candidate) then
    writeln (candidate, ' is a perfect number.');
end.
