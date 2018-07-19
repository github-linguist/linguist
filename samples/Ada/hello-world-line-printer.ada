with Ada.Text_IO; use Ada.Text_IO;

procedure Print_Line is
   Printer : File_Type;
begin
   begin
      Open (Printer, Mode => Out_File, Name => "/dev/lp0");
   exception
      when others =>
         Put_Line ("Unable to open printer.");
         return;
   end;

   Set_Output (Printer);
   Put_Line ("Hello World!");
   Close (Printer);
end Print_Line;
