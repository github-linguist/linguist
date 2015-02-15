with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;
with Ada.Text_IO; use Ada.Text_IO;

procedure Trig is
   Degrees_Cycle : constant Float := 360.0;
   Radians_Cycle : constant Float := 2.0 * Ada.Numerics.Pi;
   Angle_Degrees : constant Float := 45.0;
   Angle_Radians : constant Float := Ada.Numerics.Pi / 4.0;
   procedure Put (V1, V2 : Float) is
   begin
      Put (V1, Aft => 5, Exp => 0);
      Put (" ");
      Put (V2, Aft => 5, Exp => 0);
      New_Line;
   end Put;
begin
   Put (Sin (Angle_Degrees, Degrees_Cycle),
        Sin (Angle_Radians, Radians_Cycle));
   Put (Cos (Angle_Degrees, Degrees_Cycle),
        Cos (Angle_Radians, Radians_Cycle));
   Put (Tan (Angle_Degrees, Degrees_Cycle),
        Tan (Angle_Radians, Radians_Cycle));
   Put (Cot (Angle_Degrees, Degrees_Cycle),
        Cot (Angle_Radians, Radians_Cycle));
   Put (ArcSin (Sin (Angle_Degrees, Degrees_Cycle), Degrees_Cycle),
        ArcSin (Sin (Angle_Radians, Radians_Cycle), Radians_Cycle));
   Put (Arccos (Cos (Angle_Degrees, Degrees_Cycle), Degrees_Cycle),
        Arccos (Cos (Angle_Radians, Radians_Cycle), Radians_Cycle));
   Put (Arctan (Y => Tan (Angle_Degrees, Degrees_Cycle)),
        Arctan (Y => Tan (Angle_Radians, Radians_Cycle)));
   Put (Arccot (X => Cot (Angle_Degrees, Degrees_Cycle)),
        Arccot (X => Cot (Angle_Degrees, Degrees_Cycle)));
end Trig;
