with Ada.Text_IO; use  Ada.Text_IO;

procedure Reverse_Video is

   Rev_Video  : String := Ascii.ESC & "[7m";
   Norm_Video : String := Ascii.ESC & "[m";

begin
   Put (Rev_Video & "Reversed");
   Put (Norm_Video & " Normal");
end Reverse_Video;
