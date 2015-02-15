program in,out;

type

   pString = ^string;

var

   s1,s2 : string ;
   pStr  : pString ;

begin

   /* direct copy */
   s1 := 'Now is the time for all good men to come to the aid of their party.'
   s2 := s1 ;

   writeln(s1);
   writeln(s2);

   /* By Reference */
   pStr := @s1 ;
   writeln(pStr^);

   pStr := @s2 ;
   writeln(pStr^);

end;
