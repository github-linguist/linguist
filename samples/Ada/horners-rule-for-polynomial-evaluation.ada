with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Horners_Rule is
   type Coef is array(Positive range <>) of Float;

   function Horner(Coeffs: Coef; Val: Float) return Float is
      Res : Float := 0.0;
   begin
      for P in reverse Coeffs'Range loop
         Res := Res*Val + Coeffs(P);
      end loop;
      return Res;
   end Horner;

begin
   Put(Horner(Coeffs => (-19.0, 7.0, -4.0, 6.0), Val => 3.0), Aft=>1, Exp=>0);
end Horners_Rule;
