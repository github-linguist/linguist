with Ada.Text_IO;

procedure Mod_Inv is

   procedure X_GCD(A, B: in Natural; D, X, Y: out Integer) is
      -- the Extended Euclidean Algorithm
      -- finds (D, X, Y) with D = GCD(A, B) = A*X + B*Y
      R: Natural := A mod B;
   begin
      if R=0 then
         D := B;
         X := 0;
         Y := 1;
      else
         X_GCD(B, R, D, Y, X);
         Y := Y - (A/B)*X;
      end if;
   end X_GCD;

   function Inverse(A, M: Integer) return Integer is
      -- computes the multiplicative inverse of A mod M, using X_GCD
      Result, GCD, Dummy: Integer;
   begin
      X_GCD(A, M, GCD, Result, Dummy);
      if GCD /= 1 then -- inverse does not exist!
         raise Constraint_Error with
           "GCD (" & Integer'Image(A) & "," & Integer'Image(M) & " ) =" &
           Integer'Image(GCD) & " /= 1";
      else -- make sure Result is in {0, ..., M-1}
         if Result < 0 then
            return Result+M;
         else
            return Result;
         end if;
      end if;
   end Inverse;

begin
   Ada.Text_IO.Put_Line(Natural'Image(Inverse(42, 2017)));
   -- Ada.Text_IO.Put_Line(Natural'Image(Inverse(154, 3311)));
   -- The above would raise CONSTRAINT_ERROR : GCD ( 154, 3311 ) = 77 /= 1
end Mod_Inv;
