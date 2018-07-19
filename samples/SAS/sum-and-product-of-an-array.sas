data _null_;
   array a{*} a1-a100;
   do i=1 to 100;
      a{i}=i*i;
   end;
   b=sum(of a{*});
   put b c;
run;
