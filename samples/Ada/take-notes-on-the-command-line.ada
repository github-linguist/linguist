with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Text_IO;
procedure Notes is
   Notes_Filename : constant String := "notes.txt";
   Notes_File     : Ada.Text_IO.File_Type;
   Argument_Count : Natural         := Ada.Command_Line.Argument_Count;
begin
   if Argument_Count = 0 then
      begin
         Ada.Text_IO.Open
           (File => Notes_File,
            Mode => Ada.Text_IO.In_File,
            Name => Notes_Filename);
         while not Ada.Text_IO.End_Of_File (File => Notes_File) loop
            Ada.Text_IO.Put_Line (Ada.Text_IO.Get_Line (File => Notes_File));
         end loop;
      exception
         when Ada.IO_Exceptions.Name_Error =>
            null;
      end;
   else
      begin
         Ada.Text_IO.Open
           (File => Notes_File,
            Mode => Ada.Text_IO.Append_File,
            Name => Notes_Filename);
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Ada.Text_IO.Create (File => Notes_File, Name => Notes_Filename);
      end;
      Ada.Text_IO.Put_Line
        (File => Notes_File,
         Item => Ada.Calendar.Formatting.Image (Date => Ada.Calendar.Clock));
      Ada.Text_IO.Put (File => Notes_File, Item => Ada.Characters.Latin_1.HT);
      for I in 1 .. Argument_Count loop
         Ada.Text_IO.Put
           (File => Notes_File,
            Item => Ada.Command_Line.Argument (I));
         if I /= Argument_Count then
            Ada.Text_IO.Put (File => Notes_File, Item => ' ');
         end if;
      end loop;
      Ada.Text_IO.Flush (File => Notes_File);
   end if;
   if Ada.Text_IO.Is_Open (File => Notes_File) then
      Ada.Text_IO.Close (File => Notes_File);
   end if;
end Notes;
