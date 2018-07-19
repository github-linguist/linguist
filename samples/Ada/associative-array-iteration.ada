with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Containers.Indefinite_Ordered_Maps;

procedure Test_Iteration is
   package String_Maps is
      new Ada.Containers.Indefinite_Ordered_Maps (String, Integer);
   use String_Maps;
   A     : Map;
   Index : Cursor;
begin
   A.Insert ("hello", 1);
   A.Insert ("world", 2);
   A.Insert ("!",     3);
   Index := A.First;
   while Index /= No_Element loop
      Put_Line (Key (Index) & Integer'Image (Element (Index)));
      Index := Next (Index);
   end loop;
end Test_Iteration;
