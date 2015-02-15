with Ada.Text_IO;

procedure Single_Instance is

   package IO renames Ada.Text_IO;
   Lock_File: IO.File_Type;
   Lock_File_Name: String := "single_instance.magic_lock";

begin
   begin
      IO.Open(File => Lock_File, Mode=> IO.In_File, Name => Lock_File_Name);
      IO.Close(Lock_File);
      IO.Put_Line("I can't -- another instance of me is running ...");
   exception
      when IO.Name_Error =>
         IO.Put_Line("I can run!");
         IO.Create(File => Lock_File, Name => Lock_File_Name);
         for I in 1 .. 10 loop
            IO.Put(Integer'Image(I));
            delay 1.0; -- wait one second
         end loop;
         IO.Delete(Lock_File);
         IO.New_Line;
         IO.Put_Line("I am done!");
   end;
exception
   when others => IO.Delete(Lock_File);
end Single_Instance;
