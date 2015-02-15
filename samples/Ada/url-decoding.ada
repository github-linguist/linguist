with AWS.URL;
with Ada.Text_IO; use Ada.Text_IO;
procedure Decode is
   Encoded : constant String := "http%3A%2F%2Ffoo%20bar%2F";
begin
   Put_Line (AWS.URL.Decode (Encoded));
end Decode;
