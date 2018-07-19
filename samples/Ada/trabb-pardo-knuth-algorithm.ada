with Ada.Text_IO, Ada.Numerics.Generic_Elementary_Functions;

procedure Trabb_Pardo_Knuth is

   type Real is digits 6 range -400.0 .. 400.0;

   package TIO renames Ada.Text_IO;
   package FIO is new TIO.Float_IO(Real);
   package Math is new  Ada.Numerics.Generic_Elementary_Functions(Real);

   function F(X: Real) return Real is
   begin
      return (Math.Sqrt(abs(X)) + 5.0 * X**3);
   end F;

   Values: array(1 .. 11) of Real;

begin
   TIO.Put("Please enter 11 Numbers:");
   for I in Values'Range loop
      FIO.Get(Values(I));
   end loop;

   for I in reverse Values'Range loop
      TIO.Put("f(");
      FIO.Put(Values(I), Fore => 2, Aft => 3, Exp => 0);
      TIO.Put(")=");
      begin
         FIO.Put(F(Values(I)), Fore=> 4, Aft => 3, Exp => 0);
      exception
         when Constraint_Error => TIO.Put("-->too large<--");
      end;
      TIO.New_Line;
   end loop;

end Trabb_Pardo_Knuth;
