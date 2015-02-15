with Ada.Float_Text_IO, Ada.Text_IO;  use Ada.Float_Text_IO, Ada.Text_IO;

procedure Temperatur_Conversion is
   K: Float;
   function C return Float is (K - 273.15);
   function F return Float is (K * 1.8 - 459.67);
   function R return Float is (K * 1.8);
begin
   Get(K); New_Line;                                           -- Format
   Put("K: "); Put(K, Fore => 4, Aft => 2, Exp => 0); New_Line;-- K: dddd.dd
   Put("C: "); Put(C, Fore => 4, Aft => 2, Exp => 0); New_Line;-- C: dddd.dd
   Put("F: "); Put(F, Fore => 4, Aft => 2, Exp => 0); New_Line;-- F: dddd.dd
   Put("R: "); Put(R, Fore => 4, Aft => 2, Exp => 0); New_Line;-- R: dddd.dd
end;
