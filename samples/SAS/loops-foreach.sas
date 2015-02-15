/* Initialize an array with integers 1 to 10, and print their sum */
data _null_;
array a a1-a10;
n=1;
do over a;
  a=n;
  n=n+1;
end;
s=sum(of a{*});
put s;
run;
