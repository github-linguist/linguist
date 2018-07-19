data _null_;
   a="abracadabra";
   b=substr(a,2,3); /* first number is position, starting at 1,
                       second number is length */
   put _all_;
run;
