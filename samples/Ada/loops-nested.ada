with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Test_Loop_Nested is
   type Value_Type is range 1..20;
   package Random_Values is new Ada.Numerics.Discrete_Random (Value_Type);
   use Random_Values;
   Dice : Generator;
   A : array (1..10, 1..10) of Value_Type :=
          (others => (others => Random (Dice)));
begin

Outer :
   for I in A'Range (1) loop
      for J in A'Range (2) loop
         Put (Value_Type'Image (A (I, J)));
         exit Outer when A (I, J) = 20;
      end loop;
      New_Line;
   end loop Outer;
end Test_Loop_Nested;
