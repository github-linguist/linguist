with Ada.Text_IO, Ada.Numerics.Generic_Elementary_Functions;

procedure Mean_Angles is

   type X_Real is digits 4;      -- or more digits for improved precision
   subtype Real is X_Real range 0.0 .. 360.0; -- the range of interest
   type Angles is array(Positive range <>) of Real;

   procedure Put(R: Real) is
      package IO is new Ada.Text_IO.Float_IO(Real);
   begin
      IO.Put(R, Fore => 3, Aft => 2, Exp => 0);
   end Put;

   function Mean_Angle(A: Angles) return Real is
      Sin_Sum, Cos_Sum: X_Real := 0.0; -- X_Real since sums might exceed 360.0
      package Math is new Ada.Numerics.Generic_Elementary_Functions(Real);
      use Math;
   begin
      for I in A'Range loop
        Sin_Sum := Sin_Sum + Sin(A(I), Cycle => 360.0);
        Cos_Sum := Cos_Sum + Cos(A(I), Cycle => 360.0);
      end loop;
      return Arctan(Sin_Sum / X_Real(A'Length), Cos_Sum / X_Real(A'Length),
                    Cycle => 360.0);
        -- may raise Ada.Numerics.Argument_Error if inputs are
        -- numerically instable, e.g., when Cos_Sum is 0.0
   end Mean_Angle;

begin
   Put(Mean_Angle((10.0, 20.0, 30.0)));     Ada.Text_IO.New_Line;    -- 20.00
   Put(Mean_Angle((10.0, 350.0)));          Ada.Text_IO.New_Line;    --  0.00
   Put(Mean_Angle((90.0, 180.0, 270.0, 360.0))); -- Ada.Numerics.Argument_Error!
end Mean_Angles;
