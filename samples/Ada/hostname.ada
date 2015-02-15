with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets;

procedure Demo is
begin
   Put_Line (GNAT.Sockets.Host_Name);
end Demo;
