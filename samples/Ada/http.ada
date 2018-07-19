with Ada.Text_IO; use Ada.Text_IO;

with AWS.Client;
with AWS.Response;

procedure HTTP_Request is
begin
   Put_Line (AWS.Response.Message_Body (AWS.Client.Get (URL => "http://www.rosettacode.org")));
end HTTP_Request;
