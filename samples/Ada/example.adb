-- Example Ada implementation file (.adb).
-- Just example code.
-- Don't think of this code as a role model.

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Numerics;      use Ada.Numerics;
with CustomModule.Mine; use CustomModule.Mine;

package body Example is

   function Hypot (X, Y : Integer) return Float is
      Result : Float;
   begin
      Result := Ada.Numerics.Elementary_Functions.Sqrt(X * X + Y * Y);
      return Result;
   end;

   procedure Shift (Dx, Dy : Integer, R : in out Record_Type) is
   begin
      R.X := R.X + Dx;
      R.Y := R.Y + Dy;
   end;

end;

