with Ada.Text_Io; use Ada.Text_Io;

procedure Gcd_Test is
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

begin
   Put_Line("GCD of 100, 5 is" & Integer'Image(Gcd(100, 5)));
   Put_Line("GCD of 5, 100 is" & Integer'Image(Gcd(5, 100)));
   Put_Line("GCD of 7, 23 is" & Integer'Image(Gcd(7, 23)));
end Gcd_Test;
