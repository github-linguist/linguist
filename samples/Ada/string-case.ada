with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_Io; use Ada.Text_Io;

procedure Upper_Case_String is
   S : String := "alphaBETA";
begin
   Put_Line(To_Upper(S));
   Put_Line(To_Lower(S));
end Upper_Case_String;
