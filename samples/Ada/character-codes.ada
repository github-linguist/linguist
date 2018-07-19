with Ada.Text_IO;  use Ada.Text_IO;

procedure Char_Code is
begin
   Put_Line (Character'Val (97) & " =" & Integer'Image (Character'Pos ('a')));
end Char_Code;
