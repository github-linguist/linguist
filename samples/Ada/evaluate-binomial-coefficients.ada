with Ada.Text_IO;  use Ada.Text_IO;
procedure Test_Binomial is
   function Binomial (N, K : Natural) return Natural is
      Result : Natural := 1;
      M      : Natural;
   begin
      if N < K then
         raise Constraint_Error;
      end if;
      if K > N/2 then -- Use symmetry
         M := N - K;
      else
         M := K;
      end if;
      for I in 1..M loop
         Result := Result * (N - M + I) / I;
      end loop;
      return Result;
   end Binomial;
begin
   for N in 0..17 loop
      for K in 0..N loop
         Put (Integer'Image (Binomial (N, K)));
      end loop;
      New_Line;
   end loop;
end Test_Binomial;
