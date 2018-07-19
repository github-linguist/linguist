with AWS.URL;
with Ada.Text_IO; use Ada.Text_IO;
procedure Encode is
   Normal : constant String := "http://foo bar/";
begin
   Put_Line (AWS.URL.Encode (Normal));
end Encode;
