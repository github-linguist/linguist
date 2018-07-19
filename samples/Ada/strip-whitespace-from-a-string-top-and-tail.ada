with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
procedure StripDemo is
   str : String := "     Jabberwocky     ";
begin
   Put_Line ("'" & Trim (str, Left) & "'");
   Put_Line ("'" & Trim (str, Right) & "'");
   Put_Line ("'" & Trim (str, Both) & "'");
end StripDemo;
