with Ada.Text_IO, GNAT.Bubble_Sort;
use  Ada.Text_IO;

procedure DisjointSort is

   package Int_Io is new Integer_IO (Integer);

   subtype Index_Range is Natural range 1 .. 8;
   Input_Array : array (Index_Range) of Integer := (7, 6, 5, 4, 3, 2, 1, 0);

   subtype Subindex_Range is Natural range 1 .. 3;
   type Sub_Arrays is array (Subindex_Range) of Integer;

   Sub_Index : Sub_Arrays := (7, 2, 8);
   Sub_Array : Sub_Arrays;

   -- reuse of the somehow generic GNAT.Bubble_Sort (for Ada05)

   procedure Sort (Work_Array : in out Sub_Arrays) is
      procedure Exchange (Op1, Op2 : Natural) is
         Temp : Integer;
      begin
         Temp             := Work_Array (Op1);
         Work_Array (Op1) := Work_Array (Op2);
         Work_Array (Op2) := Temp;
      end Exchange;

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return (Work_Array (Op1) < Work_Array (Op2));
      end Lt;
   begin
      GNAT.Bubble_Sort.Sort
        (N    => Subindex_Range'Last,
         Xchg => Exchange'Unrestricted_Access,
         Lt   => Lt'Unrestricted_Access);
   end Sort;

begin
   -- as the positions are not ordered, first sort the positions
   Sort (Sub_Index);
   -- extract the values to be sorted
   for I in Subindex_Range loop
      Sub_Array (I) := Input_Array (Sub_Index (I));
   end loop;
   Sort (Sub_Array);
   -- put the sorted values at the right place
   for I in Subindex_Range loop
      Input_Array (Sub_Index (I))  := Sub_Array (I);
   end loop;

   for I in Index_Range loop
      Int_Io.Put (Input_Array (I), Width => 2);
   end loop;
   New_Line;

end DisjointSort;
