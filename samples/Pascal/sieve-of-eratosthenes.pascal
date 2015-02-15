program primes(output)

const
 PrimeLimit = 1000;

var
 primes: set of 1 .. PrimeLimit;
 n, k: integer;
 needcomma: boolean;

begin
 { calculate the primes }
 primes := [2 .. PrimeLimit];
 for n := 1 to trunc(sqrt(PrimeLimit)) do
  begin
   if n in primes
    then
     begin
      k := n*n;
      while k < PrimeLimit do
       begin
        primes := primes - [k];
        k := k + n
       end
     end
  end;

  { output the primes }
  needcomma := false;
  for n := 1 to PrimeLimit do
   if n in primes
    then
     begin
      if needcomma
       then
        write(', ');
      write(n);
      needcomma := true
     end
end.
