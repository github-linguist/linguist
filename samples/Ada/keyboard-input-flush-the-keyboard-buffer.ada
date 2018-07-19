with Ada.Text_IO;
procedure Flushtest is
   use Text_IO;
begin
   Put_Line ("Type anything for 2 s");
   delay 2.0;
Flush_Input:
   declare
      Ch   : Character;
      More : Boolean;
   begin
      loop
         Get_Immediate (Ch, More);
         exit when not More;
      end loop;
   end Flush_Input;
   New_Line;
   Put_Line ("Okay, thanks. Here is some input from you:");
   Put_Line (Get_Line);
end Flushtest;
