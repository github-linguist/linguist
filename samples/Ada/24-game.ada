with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
procedure Game_24 is
   subtype Operation is Character;
   type Op_Array is array (Positive range <>) of Operation;
   type Digit is range 1 .. 9;
   type Digit_Array is array (Positive range <>) of Digit;
   package Digit_IO is new Ada.Text_IO.Integer_IO (Digit);
   package Random_Digit is new Ada.Numerics.Discrete_Random (Digit);
   Digit_Generator : Random_Digit.Generator;
   Given_Digits : array (1 .. 4) of Digit;
begin
   Ada.Text_IO.Put_Line ("24 Game");
   Ada.Text_IO.Put_Line ("Generating 4 digits...");
   Random_Digit.Reset (Digit_Generator);
   for I in Given_Digits'Range loop
      Given_Digits (I) := Random_Digit.Random (Digit_Generator);
   end loop;
   Ada.Text_IO.Put ("Your Digits:");
   for I in Given_Digits'Range loop
      Digit_IO.Put (Given_Digits (I));
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("Enter your Expression: ");
   declare
      Value : Integer;
      Input_Operations : Op_Array (1 .. 3);
      Input_Digits : Digit_Array (1 .. 4);
      Unused_Digits : array (Given_Digits'Range) of Boolean :=
        (others => True);
   begin
      -- get input
      for I in 1 .. 4 loop
         Digit_IO.Get (Input_Digits (I));
         exit when I = 4;
         Ada.Text_IO.Get (Input_Operations (I));
      end loop;
      -- check input
      for I in Input_Digits'Range loop
         declare
            Found : Boolean := False;
         begin
            for J in Given_Digits'Range loop
               if Unused_Digits (J) and then
                 Given_Digits (J) = Input_Digits (I) then
                  Unused_Digits (J) := False;
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               Ada.Text_IO.Put_Line ("Illegal Number used:" &
                                     Digit'Image (Input_Digits (I)));
               return;
            end if;
         end;
      end loop;
      -- check value
      Value := Integer (Input_Digits (Input_Digits'First));
      for I in Input_Operations'Range loop
         case Input_Operations (I) is
            when '+' =>
               Value := Value + Integer (Input_Digits (I + 1));
            when '-' =>
               Value := Value - Integer (Input_Digits (I + 1));
            when '*' =>
               Value := Value * Integer (Input_Digits (I + 1));
            when '/' =>
               Value := Value / Integer (Input_Digits (I + 1));
            when others =>
               Ada.Text_IO.Put_Line ("Illegal Op used:" &
                                     Input_Operations (I));
               return;
         end case;
      end loop;
      if Value /= 24 then
         Ada.Text_IO.Put_Line ("Value" & Integer'Image (Value) &
                               " is not 24!");
      else
         Ada.Text_IO.Put_Line ("You won!");
      end if;
   end;
end Game_24;
