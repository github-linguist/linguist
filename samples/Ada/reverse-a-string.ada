with Ada.Text_IO; use Ada.Text_IO;

procedure Reverse_String is
   function Reverse_It (Item : String) return String is
      Result : String (Item'Range);
   begin
      for I in Item'range loop
         Result (Result'Last - I + Item'First) := Item (I);
      end loop;
      return Result;
   end Reverse_It;
begin
   Put_Line (Reverse_It (Get_Line));
end Reverse_String;
