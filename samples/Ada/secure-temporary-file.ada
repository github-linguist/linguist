with Ada.Text_Io; use Ada.Text_Io;

procedure Temp_File is
   Temp : File_Type;
   Contents : String(1..80);
   Length : Natural;
begin
   -- Create a temporary file
   Create(File => Temp);
   Put_Line(File => Temp, Item => "Hello World");
   Reset(File => Temp, Mode => In_File);
   Get_Line(File => Temp, Item => Contents, Last => Length);
   Put_Line(Contents(1..Length));
end Temp_File;
