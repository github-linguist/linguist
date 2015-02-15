with Ada.Text_IO;

procedure Cursor_Pos is

begin
   Ada.Text_IO.Set_Line(6);
   Ada.Text_IO.Set_Col(3);
   Ada.Text_IO.Put("Hello");
end Cursor_Pos;
