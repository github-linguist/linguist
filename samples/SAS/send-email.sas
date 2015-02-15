filename msg email
   to="afriend@someserver.com"
   cc="anotherfriend@somecompany.com"
   subject="Important message"
;

data _null_;
   file msg;
   put "Hello, Connected World!";
run;
