with Ada.Text_IO, Ada.Numerics.Float_Random;

procedure One_Of_N is

   Num_Of_Lines: constant Positive := 10;

   package Rnd renames Ada.Numerics.Float_Random;
   Gen: Rnd.Generator; -- used globally

   function Choose_One_Of_N(Last_Line_Number: Positive) return Natural is
      Current_Choice: Natural := 0;
   begin
      for Line_Number in 1 .. Last_Line_Number loop
        if (Rnd.Random(Gen) * Float(Line_Number) <= 1.0) then
           Current_Choice := Line_Number;
        end if;
      end loop;
      return Current_Choice;
   end Choose_One_Of_N;

   Results: array(1 .. Num_Of_Lines) of Natural := (others => 0);
   Index: Integer range 1 .. Num_Of_Lines;

begin
   Rnd.Reset(Gen);
   for I in 1 .. 1_000_000 loop    -- compute results
      Index := Choose_One_Of_N(Num_Of_Lines);
      Results(Index) := Results(Index) + 1;
   end loop;

   for R in Results'Range loop    -- output results
      Ada.Text_IO.Put(Integer'Image(Results(R)));
   end loop;
end One_Of_N;
