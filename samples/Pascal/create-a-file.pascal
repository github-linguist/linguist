program in out;

var

   f : textfile;

begin

   assignFile(f,'/output.txt');
   rewrite(f);
   close(f);
   makedir('/docs');
   assignFile(f,'/docs/output.txt');
   rewrite(f);
   close(f);

end;
