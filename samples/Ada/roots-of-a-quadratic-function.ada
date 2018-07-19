with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;

procedure Quadratic_Equation is
   type Roots is array (1..2) of Float;
   function Solve (A, B, C : Float) return Roots is
      SD : constant Float := sqrt (B**2 - 4.0 * A * C);
      X  : Float;
   begin
      if B < 0.0 then
         X := (- B + SD) / 2.0 * A;
         return (X, C / (A * X));
      else
         X := (- B - SD) / 2.0 * A;
         return (C / (A * X), X);
      end if;
   end Solve;

   R : constant Roots := Solve (1.0, -10.0E5, 1.0);
begin
   Put_Line ("X1 =" & Float'Image (R (1)) & " X2 =" & Float'Image (R (2)));
end Quadratic_Equation;
