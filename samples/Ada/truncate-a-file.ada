with Ada.Command_Line, Ada.Sequential_IO, Ada.Directories;

procedure Truncate_File is

   type Byte is mod 256;
   for Byte'Size use 8;

   package Byte_IO is new Ada.Sequential_IO(Byte);

   function Arg(N: Positive) return String renames Ada.Command_Line.Argument;
   function Args return Natural renames Ada.Command_Line.Argument_Count;

begin
   -- give help output if neccessary
   if Args < 2 or else Args > 3 then
      raise Program_Error
        with "usage: truncate_file <filename> <length> [<temp_file>]";
   end if;

   -- now do the real work
   declare
      File_Name: String := Arg(1);
      Temp_Name: String := (if Args = 2 then Arg(1) & ".tmp" else Arg(3));
                             -- an Ada 2012 conditional expression
      File, Temp: Byte_IO.File_Type;
      Count: Natural := Natural'Value(Arg(2));
      Value: Byte;
   begin
      -- open files
      Byte_IO.Open  (File => File, Mode => Byte_IO.In_File,  Name => File_Name);
      Byte_IO.Create(File => Temp, Mode => Byte_IO.Out_File, Name => Temp_Name);

      -- copy the required bytes (but at most as much as File has) from File to Temp
      while (not Byte_IO.End_Of_File(File)) and Count > 0 loop
         Byte_IO.Read (File, Value);
         Byte_IO.Write(Temp, Value);
         Count := Count - 1;
      end loop;

      -- close files
      Byte_IO.Close(Temp);
      Byte_IO.Close(File);

      if Count = 0 then -- File was at least Count bytes long
         -- remove File and rename Temp to File
         Ada.Directories.Delete_File(Name => File_Name);
         Ada.Directories.Rename(Old_Name => Temp_Name, New_Name => File_Name);
      else -- File was too short
         -- remove Temp and leave File as it is, output error
         Ada.Directories.Delete_File(Name => Temp_Name);
         raise Program_Error
           with "Size of """ & File_Name & """ less than " & Arg(2);
      end if;
   end;
end Truncate_File;
