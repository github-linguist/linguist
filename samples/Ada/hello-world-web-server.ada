with AWS; use AWS;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with Ada.Text_IO; use Ada.Text_IO;
procedure HelloHTTP is
   function CB (Request : Status.Data) return Response.Data is
      pragma Unreferenced (Request);
   begin
      return Response.Build ("text/html", "Hello world!");
   end CB;
   TheServer : Server.HTTP;
   ch : Character;
begin
   Server.Start (TheServer, "Rosettacode",
      Callback => CB'Unrestricted_Access, Port => 8080);
   Put_Line ("Press any key to quit."); Get_Immediate (ch);
   Server.Shutdown (TheServer);
end HelloHTTP;
