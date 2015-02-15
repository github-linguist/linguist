data _null_;
do while(1);
   n=floor(uniform(0)*20);
   put n;
   if n=10 then leave;    /* 'leave' to break a loop */
end;
run;
