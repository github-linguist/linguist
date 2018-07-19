with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Random_Distribution is
   Trials : constant := 1_000_000;
   type Outcome is (Aleph, Beth, Gimel, Daleth, He, Waw, Zayin, Heth);
   Pr : constant array (Outcome) of Uniformly_Distributed :=
        (1.0/5.0, 1.0/6.0, 1.0/7.0, 1.0/8.0, 1.0/9.0, 1.0/10.0, 1.0/11.0, 1.0);
   Samples : array (Outcome) of Natural := (others => 0);
   Value   : Uniformly_Distributed;
   Dice    : Generator;
begin
   for Try in 1..Trials loop
      Value := Random (Dice);
      for I in Pr'Range loop
         if Value <= Pr (I) then
            Samples (I) := Samples (I) + 1;
            exit;
         else
            Value := Value - Pr (I);
         end if;
      end loop;
   end loop;
      -- Printing the results
   for I in Pr'Range loop
      Put (Outcome'Image (I) & Character'Val (9));
      Put (Float'Image (Float (Samples (I)) / Float (Trials)) & Character'Val (9));
      if I = Heth then
         Put_Line (" rest");
      else
         Put_Line (Uniformly_Distributed'Image (Pr (I)));
      end if;
   end loop;
end Random_Distribution;
