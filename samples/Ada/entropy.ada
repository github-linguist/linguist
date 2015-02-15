with Ada.Text_IO, Ada.Float_Text_IO, Ada.Numerics.Elementary_Functions;

procedure Count_Entropy is

   package TIO renames Ada.Text_IO;

   Count: array(Character) of Natural := (others => 0);
   Sum:   Natural := 0;
   Line: String := "1223334444";

begin
   for I in Line'Range loop   -- count the characters
      Count(Line(I)) := Count(Line(I))+1;
      Sum := Sum + 1;
   end loop;

   declare   -- compute the entropy and print it
      function P(C: Character) return Float is (Float(Count(C)) / Float(Sum));
      use Ada.Numerics.Elementary_Functions, Ada.Float_Text_IO;
      Result: Float := 0.0;
   begin
      for Ch in Character loop
         Result := Result -
          (if P(Ch)=0.0 then 0.0 else P(Ch) * Log(P(Ch), Base => 2.0));
      end loop;
      Put(Result, Fore => 1, Aft => 5, Exp => 0);
   end;
end Count_Entropy;
