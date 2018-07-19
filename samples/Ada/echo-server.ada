with Ada.Text_IO;
with Ada.IO_Exceptions;
with GNAT.Sockets;
procedure Echo_Server is
   Receiver   : GNAT.Sockets.Socket_Type;
   Connection : GNAT.Sockets.Socket_Type;
   Client     : GNAT.Sockets.Sock_Addr_Type;
   Channel    : GNAT.Sockets.Stream_Access;
begin
   GNAT.Sockets.Create_Socket (Socket => Receiver);
   GNAT.Sockets.Set_Socket_Option
     (Socket => Receiver,
      Option => (Name    => GNAT.Sockets.Reuse_Address, Enabled => True));
   GNAT.Sockets.Bind_Socket
     (Socket  => Receiver,
      Address => (Family => GNAT.Sockets.Family_Inet,
                  Addr   => GNAT.Sockets.Inet_Addr ("127.0.0.1"),
                  Port   => 12321));
   GNAT.Sockets.Listen_Socket (Socket => Receiver);
   loop
      GNAT.Sockets.Accept_Socket
        (Server  => Receiver,
         Socket  => Connection,
         Address => Client);
      Ada.Text_IO.Put_Line
        ("Client connected from " & GNAT.Sockets.Image (Client));
      Channel := GNAT.Sockets.Stream (Connection);
      begin
         loop
            Character'Output (Channel, Character'Input (Channel));
         end loop;
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
      end;
      GNAT.Sockets.Close_Socket (Connection);
   end loop;
end Echo_Server;
