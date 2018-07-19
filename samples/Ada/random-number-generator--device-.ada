with Ada.Streams.Stream_IO;
with Ada.Text_IO;
procedure Random is
   Number : Integer;
   Random_File : Ada.Streams.Stream_IO.File_Type;
begin
   Ada.Streams.Stream_IO.Open (File => Random_File,
                               Mode => Ada.Streams.Stream_IO.In_File,
                               Name => "/dev/random");
   Integer'Read (Ada.Streams.Stream_IO.Stream (Random_File), Number);
   Ada.Streams.Stream_IO.Close (Random_File);
   Ada.Text_IO.Put_Line ("Number:" & Integer'Image (Number));
end Random;
