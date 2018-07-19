with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
procedure AsciiArt is
   art : constant array(1..27) of String(1..14) :=
     (1=>"    /\\\\\\   ", 2=>"          /\\\",
      3|6|9=>"              ", 4|12=>"  /\\\\\\\\\\ ",
      5|8|11=>"         \/\\\", 7|17|21=>" /\\\//////\\\",
      10|19|20|22=>"\/\\\    \/\\\", 13|23|24=>"\/\\\\\\\\\\\\",
      14|18=>"  /\\\\\\\\\\\", 15=>" \/////////\\\",
      16=>"\/\\\//////\\\", 25=>"\///     \/// ",
      26|27=>"\//////////// ");
begin
   for i in art'Range loop
      Put(art(i)&' ');
      if i mod 3 = 0 then New_Line; Put(i/3*' '); end if;
   end loop;
end AsciiArt;
