with Ada.Text_IO;
procedure Combinations is

   generic
      type Set is (<>);
   function Combinations
     (Count  : Positive;
      Output : Boolean := False)
      return   Natural;

   function Combinations
     (Count  : Positive;
      Output : Boolean := False)
      return   Natural
   is
      package Set_IO is new Ada.Text_IO.Enumeration_IO (Set);
      type Set_Array is array (Positive range <>) of Set;
      Empty_Array : Set_Array (1 .. 0);
      function Recurse_Combinations
        (Number : Positive;
         First  : Set;
         Prefix : Set_Array)
         return   Natural
      is
         Combination_Count : Natural := 0;
      begin
         for Next in First .. Set'Last loop
            if Number = 1 then
               Combination_Count := Combination_Count + 1;
               if Output then
                  for Element in Prefix'Range loop
                     Set_IO.Put (Prefix (Element));
                     Ada.Text_IO.Put ('+');
                  end loop;
                  Set_IO.Put (Next);
                  Ada.Text_IO.New_Line;
               end if;
            else
               Combination_Count := Combination_Count +
                                    Recurse_Combinations
                                       (Number - 1,
                                        Next,
                                        Prefix & (1 => Next));
            end if;
         end loop;
         return Combination_Count;
      end Recurse_Combinations;
   begin
      return Recurse_Combinations (Count, Set'First, Empty_Array);
   end Combinations;

   type Donuts is (Iced, Jam, Plain);
   function Donut_Combinations is new Combinations (Donuts);

   subtype Ten is Positive range 1 .. 10;
   function Ten_Combinations is new Combinations (Ten);

   Donut_Count : constant Natural :=
      Donut_Combinations (Count => 2, Output => True);
   Ten_Count   : constant Natural := Ten_Combinations (Count => 3);
begin
   Ada.Text_IO.Put_Line ("Total Donuts:" & Natural'Image (Donut_Count));
   Ada.Text_IO.Put_Line ("Total Tens:" & Natural'Image (Ten_Count));
end Combinations;
