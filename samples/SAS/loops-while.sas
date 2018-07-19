data _null_;
n=1024;
do while(n>0);
  put n;
  n=int(n/2);
end;
run;
