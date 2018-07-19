with Ada.Text_IO;
procedure Radix_Sort is
   type Integer_Array is array (Positive range <>) of Integer;

   procedure Least_Significant_Radix_Sort (Data : in out Integer_Array; Base : Positive := 10) is
      type Bucket is record
         Count   : Natural := 0;
         Content : Integer_Array (Data'Range);
      end record;

      subtype Bucket_Index is Integer range -Base + 1 .. Base - 1;
      type Bucket_Array is array (Bucket_Index) of Bucket;

      procedure Append (To : in out Bucket; Item : Integer) is
      begin
         To.Count := To.Count + 1;
         To.Content (To.Count) := Item;
      end Append;

      function Get_Nth_Digit (Value : Integer; N : Positive) return Integer is
         Result : Integer := (Value / (Base ** (N - 1))) mod Base;
      begin
         if Value < 0 then
            Result := -Result;
         end if;
         return Result;
      end Get_Nth_Digit;

      function Get_Maximum return Natural is
         Result : Natural := 0;
      begin
         for I in Data'Range loop
            if abs (Data (I)) > Result then
               Result := abs (Data (I));
            end if;
         end loop;
         return Result;
      end Get_Maximum;

      function Split (Pass : Positive) return Bucket_Array is
         Buckets : Bucket_Array;
      begin
         for I in Data'Range loop
            Append (To   => Buckets (Get_Nth_Digit (Data (I), Pass)),
                    Item => Data (I));
         end loop;
         return Buckets;
      end Split;

      function Merge (Buckets : Bucket_Array) return Integer_Array is
         Result        : Integer_Array (Data'Range);
         Current_Index : Positive := 1;
      begin
         for Sublist in Buckets'Range loop
            for Item in 1 .. Buckets (Sublist).Count loop
               Result (Current_Index) := Buckets (Sublist).Content (Item);
               Current_Index := Current_Index + 1;
            end loop;
         end loop;
         return Result;
      end Merge;

      Max_Number  : Natural := Get_Maximum;
      Digit_Count : Positive := 1;
   begin
      -- count digits of biggest number
      while Max_Number > Base loop
         Digit_Count := Digit_Count + 1;
         Max_Number := Max_Number / Base;
      end loop;
      for Pass in 1 .. Digit_Count loop
         Data := Merge (Split (Pass));
      end loop;
   end Least_Significant_Radix_Sort;

   Test_Array : Integer_Array := (170, 45, 75, -90, -802, 24, 2, 66);
begin
   Least_Significant_Radix_Sort (Test_Array, 4);
   for I in Test_Array'Range loop
      Ada.Text_IO.Put (Integer'Image (Test_Array (I)));
   end loop;
   Ada.Text_IO.New_Line;
end Radix_Sort;
