with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
procedure Guess_Number_Feedback is
   procedure Guess_Number (Lower_Limit : Integer; Upper_Limit : Integer) is
      subtype Number is Integer range Lower_Limit .. Upper_Limit;
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
         if Your_Guess > My_Number then
            Ada.Text_IO.Put_Line ("Wrong, too high!");
         else
            Ada.Text_IO.Put_Line ("Wrong, too low!");
         end if;
      end loop;
      Ada.Text_IO.Put_Line ("Well guessed!");
   end Guess_Number;
   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   Lower_Limit : Integer;
   Upper_Limit : Integer;
begin
   loop
      Ada.Text_IO.Put ("Lower Limit: ");
      Int_IO.Get (Lower_Limit);
      Ada.Text_IO.Put ("Upper Limit: ");
      Int_IO.Get (Upper_Limit);
      exit when Lower_Limit < Upper_Limit;
      Ada.Text_IO.Put_Line ("Lower limit must be lower!");
   end loop;
   Guess_Number (Lower_Limit, Upper_Limit);
end Guess_Number_Feedback;
