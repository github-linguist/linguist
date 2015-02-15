with Ada.Text_IO;  use Ada.Text_IO;

procedure String_Concatenation is
   S : String := "Hello";
begin
   Put_Line (S & " literal");
   declare
      S1 : String := S & " literal";
   begin
      Put_Line (S1);
   end;
end String_Concatenation;
