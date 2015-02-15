with Ada.Directories;          use Ada.Directories;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;

procedure File_Time_Test is
begin
   Put_Line (Image (Modification_Time ("file_time_test.adb")));
end File_Time_Test;
