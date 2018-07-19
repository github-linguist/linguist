with Ada.Text_IO;
procedure Pancake_Sort is
   generic
      type Element_Type is private;
      type Index_Type is range <>;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function ">" (Left, Right : Element_Type) return Boolean is <>;
   procedure Pancake_Sort (Data: in out Array_Type);

   procedure Pancake_Sort (Data: in out Array_Type) is
      procedure Flip (Up_To : in Index_Type) is
         Temp : constant Array_Type := Data (Data'First .. Up_To);
      begin
         for I in Temp'Range loop
            Data (I) := Temp (Temp'First + Up_To - I);
         end loop;
      end Flip;
      Max_Index : Index_Type;
   begin
      for I in reverse Data'First + 1 .. Data'Last loop
         Max_Index := Data'First;
         for A in Data'First + 1 .. I loop
            if Data(A) > Data (Max_Index) then
               Max_Index := A;
            end if;
         end loop;
         if Max_Index /= I then
            if Max_Index > Data'First then
               Flip (Max_Index);
            end if;
            Flip (I);
         end if;
      end loop;
   end Pancake_Sort;

   type Integer_Array is array (Positive range <>) of Integer;
   procedure Int_Pancake_Sort is new Pancake_Sort (Integer, Positive, Integer_Array);
   Test_Array : Integer_Array := (3, 14, 1, 5, 9, 2, 6, 3);
begin
   Int_Pancake_Sort (Test_Array);
   for I in Test_Array'Range loop
      Ada.Text_IO.Put (Integer'Image (Test_Array (I)));
   end loop;
   Ada.Text_IO.New_Line;
end Pancake_Sort;
