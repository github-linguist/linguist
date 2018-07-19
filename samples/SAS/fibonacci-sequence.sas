/* building a table with fibonacci sequence */
data fib;
a=0;
b=1;
do n=0 to 20;
   f=a;
   output;
   a=b;
   b=f+a;
end;
keep n f;
run;
