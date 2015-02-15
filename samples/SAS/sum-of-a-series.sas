data _null_;
s=0;
do n=1 to 1000;
   s+1/n**2;        /* s+x is synonym of s=s+x */
end;
e=s-constant('pi')**2/6;
put s e;
run;
