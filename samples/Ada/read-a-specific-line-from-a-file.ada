with Ada.Command_Line,
     Ada.Text_IO;

procedure Rosetta_Read is
   use Ada.Command_Line, Ada.Text_IO;

   Source : File_Type;
begin

   if Argument_Count /= 1 then
      Put_Line (File => Standard_Error,
                Item => "Usage: " & Command_Name & " file_name");
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      File_Name : String renames Argument (Number => 1);
   begin
      Open (File => Source,
            Mode => In_File,
            Name => File_Name);
   exception
      when others =>
         Put_Line (File => Standard_Error,
                   Item => "Can not open '" & File_Name & "'.");
         Set_Exit_Status (Failure);
         return;
   end;

   Set_Line (File => Source,
             To   => 7);

   declare
      Line_7 : constant String := Get_Line (File => Source);
   begin
      if Line_7'Length = 0 then
         Put_Line ("Line 7 is empty.");
      else
         Put_Line (Line_7);
      end if;
   end;
exception
   when End_Error =>
      Put_Line (File => Standard_Error,
                Item => "The file contains fewer than 7 lines.");
      Set_Exit_Status (Failure);
      return;
   when Storage_Error =>
      Put_Line (File => Standard_Error,
                Item => "Line 7 is too long to load.");
      Set_Exit_Status (Failure);
      return;
end Rosetta_Read;
