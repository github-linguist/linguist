with Ada.Command_Line, Ada.Text_IO;

procedure Command_Name is
begin
   Ada.Text_IO.Put_Line(Ada.Command_Line.Command_Name);
end Command_Name;
