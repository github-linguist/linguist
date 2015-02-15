with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Recursion_Depth is
   function Recursion (Depth : Positive) return Positive is
   begin
      return Recursion (Depth + 1);
   exception
      when Storage_Error =>
         return Depth;
   end Recursion;
begin
   Put_Line ("Recursion depth on this system is" & Integer'Image (Recursion (1)));
end Test_Recursion_Depth;
