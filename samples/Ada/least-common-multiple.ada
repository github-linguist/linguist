with Ada.Text_IO; use Ada.Text_IO;

procedure Lcm_Test is
   function Gcd (A, B : Integer) return Integer is
      M : Integer := A;
      N : Integer := B;
      T : Integer;
   begin
      while N /= 0 loop
         T := M;
         M := N;
         N := T mod N;
      end loop;
      return M;
   end Gcd;

   function Lcm (A, B : Integer) return Integer is
   begin
      if A = 0 or B = 0 then
         return 0;
      end if;
      return abs (A) * (abs (B) / Gcd (A, B));
   end Lcm;
begin
   Put_Line ("LCM of 12, 18 is" & Integer'Image (Lcm (12, 18)));
   Put_Line ("LCM of -6, 14 is" & Integer'Image (Lcm (-6, 14)));
   Put_Line ("LCM of 35, 0 is" & Integer'Image (Lcm (35, 0)));
end Lcm_Test;
