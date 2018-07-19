{ for stdio }

var

 s : string ;

begin

  repeat

    readln(s);

  until s = "" ;

{ for a file }

var

 f : text ;
 s : string ;

begin

  assignfile(f,'foo');
  reset(f);

  while not eof(f) do
    readln(f,s);

  closefile(f);

end;
