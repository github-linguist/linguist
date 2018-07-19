with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Test_Loop_Break is
   type Value_Type is range 1..20;
   package Random_Values is new Ada.Numerics.Discrete_Random (Value_Type);
   use Random_Values;
   Dice : Generator;
   A, B : Value_Type;
begin
   loop
      A := Random (Dice);
      Put_Line (Value_Type'Image (A));
      exit when A = 10;
      B := Random (Dice);
      Put_Line (Value_Type'Image (B));
   end loop;
end Test_Loop_Break;
