data primes;
do n=1 to 1000;
  link primep;
  if primep then output;
end;
stop;

primep:
if n < 4 then do;
  primep=n=2 or n=3;
  return;
end;
primep=0;
if mod(n,2)=0 then return;
do k=3 to sqrt(n) by 2;
  if mod(n,k)=0 then return;
end;
primep=1;
return;
keep n;
run;
