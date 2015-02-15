with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Bulls_And_Cows is
   package Random_Natural is new Ada.Numerics.Discrete_Random (Natural);
   Number : String (1..4);
begin
   declare -- Generation of number
      use Random_Natural;
      Digit    : String   := "123456789";
      Size     : Positive := 9;
      Dice     : Generator;
      Position : Natural;
   begin
      Reset (Dice);
      for I in Number'Range loop
         Position := Random (Dice) mod Size + 1;
         Number (I) := Digit (Position);
         Digit (Position..Size - 1) := Digit (Position + 1..Size);
         Size := Size - 1;
      end loop;
   end;
   loop -- Guessing loop
      Put ("Enter four digits:");
      declare
         Guess : String  := Get_Line;
         Bulls : Natural := 0;
         Cows  : Natural := 0;
      begin
         if Guess'Length /= 4 then
            raise Data_Error;
         end if;
         for I in Guess'Range loop
            for J in Number'Range loop
               if Guess (I) not in '1'..'9' or else (I < J and then Guess (I) = Guess (J)) then
                  raise Data_Error;
               end if;
               if Number (I) = Guess (J) then
                  if I = J then
                     Bulls := Bulls + 1;
                  else
                     Cows := Cows + 1;
                  end if;
               end if;
            end loop;
         end loop;
         exit when Bulls = 4;
         Put_Line (Integer'Image (Bulls) & " bulls," & Integer'Image (Cows) & " cows");
      exception
         when Data_Error => Put_Line ("You should enter four different digits 1..9");
      end;
   end loop;
end Bulls_And_Cows;
