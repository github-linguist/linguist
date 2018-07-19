with Ada.Numerics.Long_Elementary_Functions;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Sequence_Of_Non_Squares_Test is
   use Ada.Numerics.Long_Elementary_Functions;

   function Non_Square (N : Positive) return Positive is
   begin
      return N + Positive (Long_Float'Rounding (Sqrt (Long_Float (N))));
   end Non_Square;

   I : Positive;
begin
   for N in 1..22 loop -- First 22 non-squares
      Put (Natural'Image (Non_Square (N)));
   end loop;
   New_Line;
   for N in 1..1_000_000 loop -- Check first million of
      I := Non_Square (N);
      if I = Positive (Sqrt (Long_Float (I)))**2 then
         Put_Line ("Found a square:" & Positive'Image (N));
      end if;
   end loop;
end Sequence_Of_Non_Squares_Test;
