   function Fib (X: in Integer) return Integer is
      function Actual_Fib (N: in Integer) return Integer is
      begin
         if N < 2 then
            return N;
         else
            return Actual_Fib (N-1) + Actual_Fib (N-2);
         end if;
      end Actual_Fib;
   begin
      if X < 0 then
         raise Constraint_Error;
      else
         return Actual_Fib (X);
      end if;
   end Fib;
