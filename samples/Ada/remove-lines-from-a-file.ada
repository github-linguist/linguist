with Ada.Text_IO, Ada.Directories, Ada.Command_Line, Ada.IO_Exceptions;
use Ada.Text_IO;

procedure Remove_Lines_From_File is
   Temporary: constant String := ".tmp";
begin
   if Ada.Command_Line.Argument_Count /= 3 then
      raise Constraint_Error;
   end if;
   declare
      Filename: String := Ada.Command_Line.Argument(1);
      First: Positive := Integer'Value(Ada.Command_Line.Argument(2));
      Last: Natural := Integer'Value(Ada.Command_Line.Argument(3)) + First - 1;
      Input, Output: File_Type;
      Line_Number: Positive := 1;
   begin
      Open(Input, In_File, Filename); -- open original file for reading
      Create(Output, Out_File, Filename & Temporary); -- write to temp. file
      while not End_Of_File(Input) loop
         declare
            Line: String := Get_Line(Input);
         begin
            if Line_Number < First or else Line_Number > Last then
               Put_Line(Output, Line);
            end if;
         end;
         Line_Number := Line_Number + 1;
      end loop;
      Close(Input);
      Close(Output);
      Ada.Directories.Rename(Old_Name => Filename & Temporary,
                             New_Name => Filename);
   end;
exception
   when Constraint_Error | Ada.IO_Exceptions.Name_Error =>
      Put_Line("usage: " & Ada.Command_Line.Command_Name &
                 " <filename> <first> <length>");
      Put_Line("  opens <filename> for reading and " &
                 "<filename>" & Temporary & " for temporary writing");
      Put_Line("  requires first > 0, length >= 0");
end Remove_Lines_From_File;
