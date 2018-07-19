with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

procedure Truncatable_Primes is
   package Natural_Set is new Ada.Containers.Ordered_Sets (Natural);
   use Natural_Set;

   Primes : Set;

   function Is_Prime (N : Natural) return Boolean is
      Position : Cursor := First (Primes);
   begin
      while Has_Element (Position) loop
         if N mod Element (Position) = 0 then
            return False;
         end if;
         Position := Next (Position);
      end loop;
      return True;
   end Is_Prime;

   function Is_Left_Trucatable_Prime (N : Positive) return Boolean is
      M : Natural := 1;
   begin
      while Contains (Primes, N mod (M * 10)) and (N / M) mod 10 > 0 loop
         M := M * 10;
         if N <= M then
            return True;
         end if;
      end loop;
      return False;
   end Is_Left_Trucatable_Prime;

   function Is_Right_Trucatable_Prime (N : Positive) return Boolean is
      M : Natural := N;
   begin
      while Contains (Primes, M) and M mod 10 > 0 loop
         M := M / 10;
         if M <= 1 then
            return True;
         end if;
      end loop;
      return False;
   end Is_Right_Trucatable_Prime;

   Position : Cursor;
begin
   for N in 2..1_000_000 loop
      if Is_Prime (N) then
         Insert (Primes, N);
      end if;
   end loop;
   Position := Last (Primes);
   while Has_Element (Position) loop
      if Is_Left_Trucatable_Prime (Element (Position)) then
         Put_Line ("Largest LTP from 1..1000000:" & Integer'Image (Element (Position)));
         exit;
      end if;
      Previous (Position);
   end loop;
   Position := Last (Primes);
   while Has_Element (Position) loop
      if Is_Right_Trucatable_Prime (Element (Position)) then
         Put_Line ("Largest RTP from 1..1000000:" & Integer'Image (Element (Position)));
         exit;
      end if;
      Previous (Position);
   end loop;
end Truncatable_Primes;
