with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
procedure Guess_Number is
   subtype Number is Integer range 1 .. 10;
   package Number_IO is new Ada.Text_IO.Integer_IO (Number);
   package Number_RNG is new Ada.Numerics.Discrete_Random (Number);
   Generator  : Number_RNG.Generator;
   My_Number  : Number;
   Your_Guess : Number;
begin
   Number_RNG.Reset (Generator);
   My_Number := Number_RNG.Random (Generator);
   Ada.Text_IO.Put_Line ("Guess my number!");
   loop
      Ada.Text_IO.Put ("Your guess: ");
      Number_IO.Get (Your_Guess);
      exit when Your_Guess = My_Number;
      Ada.Text_IO.Put_Line ("Wrong, try again!");
   end loop;
   Ada.Text_IO.Put_Line ("Well guessed!");
end Guess_Number;
