-- Monty Hall Game

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;
with ada.Numerics.Discrete_Random;

procedure Monty_Stats is
   Num_Iterations : Positive := 100000;
   type Action_Type is (Stay, Switch);
   type Prize_Type is (Goat, Pig, Car);
   type Door_Index is range 1..3;
   package Random_Prize is new Ada.Numerics.Discrete_Random(Door_Index);
   use Random_Prize;
   Seed : Generator;
   Doors : array(Door_Index) of Prize_Type;

   procedure Set_Prizes is
      Prize_Index : Door_Index;
      Booby_Prize : Prize_Type := Goat;
   begin
      Reset(Seed);
      Prize_Index := Random(Seed);
      Doors(Prize_Index) := Car;
      for I in Doors'range loop
         if I /= Prize_Index then
            Doors(I) := Booby_Prize;
            Booby_Prize := Prize_Type'Succ(Booby_Prize);
         end if;
      end loop;
   end Set_Prizes;

   function Play(Action : Action_Type) return Prize_Type is
      Chosen : Door_Index := Random(Seed);
      Monty : Door_Index;
   begin
      Set_Prizes;
      for I in Doors'range loop
         if I /= Chosen and Doors(I) /= Car then
            Monty := I;
         end if;
      end loop;
      if Action = Switch then
         for I in Doors'range loop
            if I /= Monty and I /= Chosen then
               Chosen := I;
               exit;
            end if;
         end loop;
      end if;
      return Doors(Chosen);
   end Play;
   Winners : Natural;
   Pct : Float;
begin
   Winners := 0;
   for I in 1..Num_Iterations loop
      if Play(Stay) = Car then
         Winners := Winners + 1;
      end if;
   end loop;
   Put("Stay : count" & Natural'Image(Winners) & " = ");
   Pct := Float(Winners * 100) / Float(Num_Iterations);
   Put(Item => Pct, Aft => 2, Exp => 0);
   Put_Line("%");
   Winners := 0;
   for I in 1..Num_Iterations loop
      if Play(Switch) = Car then
         Winners := Winners + 1;
      end if;
   end loop;
   Put("Switch : count" & Natural'Image(Winners) & " = ");
   Pct := Float(Winners * 100) / Float(Num_Iterations);
   Put(Item => Pct, Aft => 2, Exp => 0);
   Put_Line("%");

end Monty_Stats;
