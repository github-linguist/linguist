with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

procedure Lucas_Lehmer_Test is
   type Ull is mod 2**64;
   function Mersenne(Item : Integer) return Boolean is
      S : Ull := 4;
      MP : Ull := 2**Item - 1;
   begin
      if Item = 2 then
         return True;
      else
         for I in 3..Item loop
            S := (S * S - 2) mod MP;
         end loop;
         return S = 0;
      end if;
   end Mersenne;
   Upper_Bound : constant Integer := 64;
begin
   Put_Line(" Mersenne primes:");
   for P in 2..Upper_Bound loop
      if Mersenne(P) then
         Put(" M");
         Put(Item => P, Width => 1);
      end if;
   end loop;
end Lucas_Lehmer_Test;
