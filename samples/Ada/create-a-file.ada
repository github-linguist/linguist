with Ada.Streams.Stream_IO, Ada.Directories;
use  Ada.Streams.Stream_IO, Ada.Directories;

procedure File_Creation is

   File_Handle : File_Type;

begin

   Create (File_Handle, Out_File, "output.txt");
   Close (File_Handle);
   Create_Directory("docs");
   Create (File_Handle, Out_File, "/output.txt");
   Close (File_Handle);
   Create_Directory("/docs");

end File_Creation;
