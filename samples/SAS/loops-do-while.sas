/* using DO UNTIL so that the loop executes at least once */
data _null_;
n=0;
do until(mod(n,6)=0);
  n=n+1;
  put n;
end;
run;
