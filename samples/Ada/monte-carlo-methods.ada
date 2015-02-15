with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;

procedure Test_Monte_Carlo is
   Dice : Generator;

   function Pi (Throws : Positive) return Float is
      Inside : Natural := 0;
   begin
      for Throw in 1..Throws loop
         if Random (Dice) ** 2 + Random (Dice) ** 2 <= 1.0 then
            Inside := Inside + 1;
         end if;
      end loop;
      return 4.0 * Float (Inside) / Float (Throws);
   end Pi;
begin
   Put_Line ("     10_000:" & Float'Image (Pi (     10_000)));
   Put_Line ("    100_000:" & Float'Image (Pi (    100_000)));
   Put_Line ("  1_000_000:" & Float'Image (Pi (  1_000_000)));
   Put_Line (" 10_000_000:" & Float'Image (Pi ( 10_000_000)));
   Put_Line ("100_000_000:" & Float'Image (Pi (100_000_000)));
end Test_Monte_Carlo;
