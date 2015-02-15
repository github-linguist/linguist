with Ada.Text_IO;  use Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;

procedure DNSQuerying is

   Host         : Host_Entry_Type (1, 1);
   Inet_Addr_V4 : Inet_Addr_Type (Family_Inet);
begin

   Host         := Get_Host_By_Name (Name => "www.kame.net");
   Inet_Addr_V4 := Addresses (Host);
   Put ("IPv4: " & Image (Value => Inet_Addr_V4));
end DNSQuerying;
