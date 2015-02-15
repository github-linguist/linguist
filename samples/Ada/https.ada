with AWS.Client;
with AWS.Response;
with Ada.Text_IO; use Ada.Text_IO;
procedure GetHttps is
begin
   Put_Line (AWS.Response.Message_Body (AWS.Client.Get (
      URL => "https://sourceforge.net/")));
end GetHttps;
