with Ada.Text_IO;

procedure Knapsack_Unbounded is

   type Bounty is record
      Value  : Natural;
      Weight : Float;
      Volume : Float;
   end record;

   function Min (A, B : Float) return Float is
   begin
      if A < B then
         return A;
      else
         return B;
      end if;
   end Min;

   Panacea : Bounty := (3000,  0.3, 0.025);
   Ichor   : Bounty := (1800,  0.2, 0.015);
   Gold    : Bounty := (2500,  2.0, 0.002);
   Limits  : Bounty := (   0, 25.0, 0.250);
   Best    : Bounty := (   0,  0.0, 0.000);
   Current : Bounty := (   0,  0.0, 0.000);

   Best_Amounts : array (1 .. 3) of Natural := (0, 0, 0);

   Max_Panacea : Natural := Natural (Float'Floor (Min
                              (Limits.Weight / Panacea.Weight,
                               Limits.Volume / Panacea.Volume)));
   Max_Ichor   : Natural := Natural (Float'Floor (Min
                              (Limits.Weight / Ichor.Weight,
                               Limits.Volume / Ichor.Volume)));
   Max_Gold    : Natural := Natural (Float'Floor (Min
                              (Limits.Weight / Gold.Weight,
                               Limits.Volume / Gold.Volume)));

begin
   for Panacea_Count in 0 .. Max_Panacea loop
      for Ichor_Count in 0 .. Max_Ichor loop
         for Gold_Count in 0 .. Max_Gold loop
            Current.Value  := Panacea_Count * Panacea.Value +
                              Ichor_Count * Ichor.Value +
                              Gold_Count * Gold.Value;
            Current.Weight := Float (Panacea_Count) * Panacea.Weight +
                              Float (Ichor_Count) * Ichor.Weight +
                              Float (Gold_Count) * Gold.Weight;
            Current.Volume := Float (Panacea_Count) * Panacea.Volume +
                              Float (Ichor_Count) * Ichor.Volume +
                              Float (Gold_Count) * Gold.Volume;
            if Current.Value  >  Best.Value and
               Current.Weight <= Limits.Weight and
               Current.Volume <= Limits.Volume then
               Best := Current;
               Best_Amounts := (Panacea_Count, Ichor_Count, Gold_Count);
            end if;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Maximum value:" & Natural'Image (Best.Value));
   Ada.Text_IO.Put_Line ("Panacea:" & Natural'Image (Best_Amounts (1)));
   Ada.Text_IO.Put_Line ("Ichor:  " & Natural'Image (Best_Amounts (2)));
   Ada.Text_IO.Put_Line ("Gold:   " & Natural'Image (Best_Amounts (3)));
end Knapsack_Unbounded;
