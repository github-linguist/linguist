with Ada.Text_IO;
procedure Stooge is
   type Integer_Array is array (Positive range <>) of Integer;
   procedure Swap (Left, Right : in out Integer) is
      Temp : Integer := Left;
   begin
      Left  := Right;
      Right := Temp;
   end Swap;
   procedure Stooge_Sort (List : in out Integer_Array) is
      T : Natural := List'Length / 3;
   begin
      if List (List'Last) < List (List'First) then
         Swap (List (List'Last), List (List'First));
      end if;
      if List'Length > 2 then
         Stooge_Sort (List (List'First     .. List'Last - T));
         Stooge_Sort (List (List'First + T .. List'Last));
         Stooge_Sort (List (List'First     .. List'Last - T));
      end if;
   end Stooge_Sort;
   Test_Array : Integer_Array := (1, 4, 5, 3, -6, 3, 7, 10, -2, -5, 7, 5, 9, -3, 7);
begin
   Stooge_Sort (Test_Array);
   for I in Test_Array'Range loop
      Ada.Text_IO.Put (Integer'Image (Test_Array (I)));
      if I /= Test_Array'Last then
         Ada.Text_IO.Put (", ");
      end if;
   end loop;
   Ada.Text_IO.New_Line;
end Stooge;
