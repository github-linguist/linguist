with Ada.Text_IO; use Ada.Text_IO;

procedure Long_Division is
   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   use Int_IO;

   type Degrees is range -1 .. Integer'Last;
   subtype Valid_Degrees is Degrees range 0 .. Degrees'Last;
   type Polynom is array (Valid_Degrees range <>) of Integer;

   function Degree (P : Polynom) return Degrees is
   begin
      for I in reverse P'Range loop
         if P (I) /= 0 then
            return I;
         end if;
      end loop;
      return -1;
   end Degree;

   function Shift_Right (P : Polynom; D : Valid_Degrees) return Polynom is
      Result : Polynom (0 .. P'Last + D) := (others => 0);
   begin
      Result (Result'Last - P'Length + 1 .. Result'Last) := P;
      return Result;
   end Shift_Right;

   function "*" (Left : Polynom; Right : Integer) return Polynom is
      Result : Polynom (Left'Range);
   begin
      for I in Result'Range loop
         Result (I) := Left (I) * Right;
      end loop;
      return Result;
   end "*";

   function "-" (Left, Right : Polynom) return Polynom is
      Result : Polynom (Left'Range);
   begin
      for I in Result'Range loop
         if I in Right'Range then
            Result (I) := Left (I) - Right (I);
         else
            Result (I) := Left (I);
         end if;
      end loop;
      return Result;
   end "-";

   procedure Poly_Long_Division (Num, Denom : Polynom; Q, R : out Polynom) is
      N : Polynom := Num;
      D : Polynom := Denom;
   begin
      if Degree (D) < 0 then
         raise Constraint_Error;
      end if;
      Q := (others => 0);
      while Degree (N) >= Degree (D) loop
         declare
            T : Polynom := Shift_Right (D, Degree (N) - Degree (D));
         begin
            Q (Degree (N) - Degree (D)) := N (Degree (N)) / T (Degree (T));
            T := T * Q (Degree (N) - Degree (D));
            N := N - T;
         end;
      end loop;
      R := N;
   end Poly_Long_Division;

   procedure Output (P : Polynom) is
      First : Boolean := True;
   begin
      for I in reverse P'Range loop
         if P (I) /= 0 then
            if First then
               First := False;
            else
               Put (" + ");
            end if;
            if I > 0 then
               if P (I) /= 1 then
                  Put (P (I), 0);
                  Put ("*");
               end if;
               Put ("x");
               if I > 1 then
                  Put ("^");
                  Put (Integer (I), 0);
               end if;
            elsif P (I) /= 0 then
               Put (P (I), 0);
            end if;
         end if;
      end loop;
      New_Line;
   end Output;

   Test_N : constant Polynom := (0 => -42, 1 => 0, 2 => -12, 3 => 1);
   Test_D : constant Polynom := (0 => -3, 1 => 1);
   Test_Q : Polynom (Test_N'Range);
   Test_R : Polynom (Test_N'Range);
begin
   Poly_Long_Division (Test_N, Test_D, Test_Q, Test_R);
   Put_Line ("Dividing Polynoms:");
   Put ("N: "); Output (Test_N);
   Put ("D: "); Output (Test_D);
   Put_Line ("-------------------------");
   Put ("Q: "); Output (Test_Q);
   Put ("R: "); Output (Test_R);
end Long_Division;
