with Ada.Directories;  use Ada.Directories;
with Ada.Text_IO;      use Ada.Text_IO;

procedure Test_File_Size is
begin
   Put_Line (File_Size'Image (Size ("input.txt")) & " bytes");
   Put_Line (File_Size'Image (Size ("/input.txt")) & " bytes");
end Test_File_Size;
