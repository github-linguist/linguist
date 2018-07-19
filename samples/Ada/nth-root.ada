with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Nth_Root is
   generic
      type Real is digits <>;
   function Nth_Root (Value : Real; N : Positive) return Real;

   function Nth_Root (Value : Real; N : Positive) return Real is
      type Index is mod 2;
      X : array (Index) of Real := (Value, Value);
      K : Index := 0;
   begin
      loop
         X (K + 1) := ( (Real (N) - 1.0) * X (K) + Value / X (K) ** (N-1) ) / Real (N);
         exit when X (K + 1) >= X (K);
         K := K + 1;
      end loop;
      return X (K + 1);
   end Nth_Root;

   function Long_Nth_Root is new Nth_Root (Long_Float);
begin
   Put_Line ("1024.0 10th  =" & Long_Float'Image (Long_Nth_Root (1024.0, 10)));
   Put_Line ("  27.0 3rd   =" & Long_Float'Image (Long_Nth_Root (27.0, 3)));
   Put_Line ("   2.0 2nd   =" & Long_Float'Image (Long_Nth_Root (2.0, 2)));
   Put_Line ("5642.0 125th =" & Long_Float'Image (Long_Nth_Root (5642.0, 125)));
end Test_Nth_Root;
