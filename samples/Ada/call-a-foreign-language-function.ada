with Ada.Text_IO;           use Ada.Text_IO;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

procedure Test_C_Interface is
   function strdup (s1 : Char_Array) return Chars_Ptr;
   pragma Import (C, strdup, "_strdup");

   S1 : constant String := "Hello World!";
   S2 : Chars_Ptr;
begin
   S2 := strdup (To_C (S1));
   Put_Line (Value (S2));
   Free (S2);
end Test_C_Interface;
