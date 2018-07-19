data _null_;
   a="Hello,";
   b="World!";
   c=a !! " " !! b;
   put c;
   *Alternative using the catx function;
   c=catx (" ", a, b);
   put c;
run;
