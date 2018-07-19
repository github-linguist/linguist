with Ada.Text_IO;
with Ada.Command_Line;
procedure Factors is
   Number  : Positive;
   Test_Nr : Positive := 1;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "Missing argument!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   Number := Positive'Value (Ada.Command_Line.Argument (1));
   Ada.Text_IO.Put ("Factors of" & Positive'Image (Number) & ": ");
   loop
      if Number mod Test_Nr = 0 then
         Ada.Text_IO.Put (Positive'Image (Test_Nr) & ",");
      end if;
      exit when Test_Nr ** 2 >= Number;
      Test_Nr := Test_Nr + 1;
   end loop;
   Ada.Text_IO.Put_Line (Positive'Image (Number) & ".");
end Factors;
