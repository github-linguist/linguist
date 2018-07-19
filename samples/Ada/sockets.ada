with GNAT.Sockets;  use GNAT.Sockets;

procedure SocketSend is
   procedure sendData (IP : String; Msg : String) is
      Client  : Socket_Type;
      Address : Sock_Addr_Type;
      Channel : Stream_Access;
   begin
      Create_Socket (Client);
      Address.Addr := Inet_Addr(ip);
      Address.Port := 256;
      Connect_Socket (Client, Address);
      Channel := Stream (Client);
      String'Write (Channel, Msg);
      Close_Socket (Client);
   end;
begin
   Initialize;
   sendData ("127.0.0.1","hello socket world");
end;
