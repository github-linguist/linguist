with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Knapsack_Continuous is
   package US renames Ada.Strings.Unbounded;

   type Item is record
      Name   : US.Unbounded_String;
      Weight : Float;
      Value  : Positive;
      Taken  : Float;
   end record;

   function "<" (Left, Right : Item) return Boolean is
   begin
      return Float (Left.Value) / Left.Weight <
             Float (Right.Value) / Right.Weight;
   end "<";

   type Item_Array is array (Positive range <>) of Item;

   function Total_Weight (Items : Item_Array) return Float is
      Sum : Float := 0.0;
   begin
      for I in Items'Range loop
         Sum := Sum + Items (I).Weight * Items (I).Taken;
      end loop;
      return Sum;
   end Total_Weight;

   function Total_Value (Items : Item_Array) return Float is
      Sum : Float := 0.0;
   begin
      for I in Items'Range loop
         Sum := Sum + Float (Items (I).Value) * Items (I).Taken;
      end loop;
      return Sum;
   end Total_Value;

   procedure Solve_Knapsack_Continuous
     (Items        : in out Item_Array;
      Weight_Limit : Float)
   is
   begin
      -- order items by value per weight unit
      Sorting : declare
         An_Item : Item;
         J       : Natural;
      begin
         for I in Items'First + 1 .. Items'Last loop
            An_Item := Items (I);
            J       := I - 1;
            while J in Items'Range and then Items (J) < An_Item loop
               Items (J + 1) := Items (J);
               J             := J - 1;
            end loop;
            Items (J + 1) := An_Item;
         end loop;
      end Sorting;
      declare
         Rest : Float := Weight_Limit;
      begin
         for I in Items'Range loop
            if Items (I).Weight <= Rest then
               Items (I).Taken := Items (I).Weight;
            else
               Items (I).Taken := Rest;
            end if;
            Rest := Rest - Items (I).Taken;
            exit when Rest <= 0.0;
         end loop;
      end;
   end Solve_Knapsack_Continuous;

   All_Items : Item_Array :=
     ((US.To_Unbounded_String ("beef"), 3.8, 36, 0.0),
      (US.To_Unbounded_String ("pork"), 5.4, 43, 0.0),
      (US.To_Unbounded_String ("ham"), 3.6, 90, 0.0),
      (US.To_Unbounded_String ("greaves"), 2.4, 45, 0.0),
      (US.To_Unbounded_String ("flitch"), 4.0, 30, 0.0),
      (US.To_Unbounded_String ("brawn"), 2.5, 56, 0.0),
      (US.To_Unbounded_String ("welt"), 3.7, 67, 0.0),
      (US.To_Unbounded_String ("salami"), 3.0, 95, 0.0),
      (US.To_Unbounded_String ("sausage"), 5.9, 98, 0.0));

begin
   Solve_Knapsack_Continuous (All_Items, 15.0);
   Ada.Text_IO.Put_Line
     ("Total Weight: " & Float'Image (Total_Weight (All_Items)));
   Ada.Text_IO.Put_Line
     ("Total Value:  " & Float'Image (Total_Value (All_Items)));
   Ada.Text_IO.Put_Line ("Items:");
   for I in All_Items'Range loop
      if All_Items (I).Taken > 0.0 then
         Ada.Text_IO.Put_Line
           ("   " &
            Float'Image (All_Items (I).Taken) &
            " of " &
            US.To_String (All_Items (I).Name));
      end if;
   end loop;
end Knapsack_Continuous;
