with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Selection_Sort is

   type Integer_Array is array (Positive range <>) of Integer;
   procedure Sort (A : in out Integer_Array) is
      Min  : Positive;
      Temp : Integer;
   begin
      for I in A'First..A'Last - 1 loop
         Min := I;
         for J in I + 1..A'Last loop
            if A (Min) > A (J) then
               Min := J;
            end if;
         end loop;
         if Min /= I then
            Temp    := A (I);
            A (I)   := A (Min);
            A (Min) := Temp;
         end if;
      end loop;
   end Sort;

   A : Integer_Array := (4, 9, 3, -2, 0, 7, -5, 1, 6, 8);
begin
   Sort (A);
   for I in A'Range loop
      Put (Integer'Image (A (I)) & " ");
   end loop;
end Test_Selection_Sort;
