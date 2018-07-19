with Ada.Text_Io; use Ada.Text_Io;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;

procedure Sleep is
   In_Val : Float;
begin
   Get(In_Val);
   Put_Line("Sleeping...");
   delay Duration(In_Val);
   Put_Line("Awake!");
end Sleep;
