with Ada.Text_IO;
procedure Comb_Sort is
   generic
      type Element_Type is private;
      type Index_Type is range <>;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function ">" (Left, Right : Element_Type) return Boolean is <>;
      with function "+" (Left : Index_Type; Right : Natural) return Index_Type is <>;
      with function "-" (Left : Index_Type; Right : Natural) return Index_Type is <>;
   procedure Comb_Sort (Data: in out Array_Type);

   procedure Comb_Sort (Data: in out Array_Type) is
      procedure Swap (Left, Right : in Index_Type) is
         Temp : Element_Type := Data(Left);
      begin
         Data(Left)  := Data(Right);
         Data(Right) := Temp;
      end Swap;
      Gap : Natural := Data'Length;
      Swap_Occured : Boolean;
   begin
      loop
         Gap := Natural (Float(Gap) / 1.25 - 0.5);
         if Gap < 1 then
            Gap := 1;
         end if;
         Swap_Occured := False;
         for I in Data'First .. Data'Last - Gap loop
            if Data (I) > Data (I+Gap) then
               Swap (I, I+Gap);
               Swap_Occured := True;
            end if;
         end loop;
         exit when Gap = 1 and not Swap_Occured;
      end loop;
   end Comb_Sort;

   type Integer_Array is array (Positive range <>) of Integer;
   procedure Int_Comb_Sort is new Comb_Sort (Integer, Positive, Integer_Array);
   Test_Array : Integer_Array := (1, 3, 256, 0, 3, 4, -1);
begin
   Int_Comb_Sort (Test_Array);
   for I in Test_Array'Range loop
      Ada.Text_IO.Put (Integer'Image (Test_Array (I)));
   end loop;
   Ada.Text_IO.New_Line;
end Comb_Sort;
