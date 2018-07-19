with Ada.Containers.Vectors;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Sockets;          use Sockets;

procedure Chat_Server is

   package Client_Vectors is new Ada.Containers.Vectors
     (Element_Type => Socket_FD, Index_Type => Positive);
   All_Clients : Client_Vectors.Vector;

   procedure Write (S : String) is
      procedure Output (Position : Client_Vectors.Cursor) is
         Sock : Socket_FD := Client_Vectors.Element (Position);
      begin
         Put_Line (Sock, S);
      end Output;
   begin
      All_Clients.Iterate (Output'Access);
   end Write;

   task type Client_Task is
      entry Start (FD : Socket_FD);
   end Client_Task;

   task body Client_Task is
      Sock    : Socket_FD;
      Sock_ID : Positive;
      Name    : Unbounded_String;
   begin
      select
         accept Start (FD : Socket_FD) do
            Sock := FD;
         end Start;
      or
         terminate;
      end select;

      while Name = Null_Unbounded_String loop
         Put (Sock, "Enter Name:");
         Name := To_Unbounded_String (Get_Line (Sock));
      end loop;
      Write (To_String (Name) & " joined.");
      All_Clients.Append (Sock);
      Sock_ID := All_Clients.Find_Index (Sock);
      loop
         declare
            Input : String := Get_Line (Sock);
         begin
            Write (To_String (Name) & ": " & Input);
         end;
      end loop;
   exception
      when Connection_Closed =>
         Put_Line ("Connection closed");
         Shutdown (Sock, Both);
         All_Clients.Delete (Sock_ID);
         Write (To_String (Name) & " left.");
   end Client_Task;

   Accepting_Socket : Socket_FD;
   Incoming_Socket  : Socket_FD;

   type Client_Access is access Client_Task;
   Dummy : Client_Access;
begin
   if Argument_Count /= 1 then
      Raise_Exception (Constraint_Error'Identity,
                       "Usage: " & Command_Name & " port");
   end if;
   Socket (Accepting_Socket, PF_INET, SOCK_STREAM);
   Setsockopt (Accepting_Socket, SOL_SOCKET, SO_REUSEADDR, 1);
   Bind (Accepting_Socket, Positive'Value (Argument (1)));
   Listen (Accepting_Socket);
   loop
      Put_Line ("Waiting for new connection");
      Accept_Socket (Accepting_Socket, Incoming_Socket);
      Put_Line ("New connection acknowledged");

      Dummy := new Client_Task;
      Dummy.Start (Incoming_Socket);
   end loop;
end Chat_Server;
