with Ada.Text_IO;
procedure Guess_Number_Player is
   procedure Guess_Number (Lower_Limit : Integer; Upper_Limit : Integer) is
      type Feedback is (Lower, Higher, Correct);
      package Feedback_IO is new Ada.Text_IO.Enumeration_IO (Feedback);
      My_Guess : Integer := Lower_Limit + (Upper_Limit - Lower_Limit) / 2;
      Your_Feedback : Feedback;
   begin
      Ada.Text_IO.Put_Line ("Think of a number!");
      loop
         Ada.Text_IO.Put_Line ("My guess: " & Integer'Image (My_Guess));
         Ada.Text_IO.Put ("Your answer (lower, higher, correct): ");
         Feedback_IO.Get (Your_Feedback);
         exit when Your_Feedback = Correct;
         if Your_Feedback = Lower then
            My_Guess := Lower_Limit + (My_Guess - Lower_Limit) / 2;
         else
            My_Guess := My_Guess + (Upper_Limit - My_Guess) / 2;
         end if;
      end loop;
      Ada.Text_IO.Put_Line ("I guessed well!");
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
end Guess_Number_Player;
