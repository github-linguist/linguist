with Ada.Text_Io; use Ada.Text_Io;

procedure Max_Subarray is
   type Int_Array is array (Positive range <>) of Integer;
   Empty_Error : Exception;
   function Max(Item : Int_Array) return Int_Array is
      Start : Positive;
      Finis : Positive;
      Max_Sum : Integer := Integer'First;
      Sum : Integer;
   begin
      if Item'Length = 0 then
         raise Empty_Error;
      end if;

      for I in Item'range loop
         Sum := 0;
         for J in I..Item'Last loop
            Sum := Sum + Item(J);
            if Sum > Max_Sum then
               Max_Sum := Sum;
               Start := I;
               Finis := J;
            end if;
         end loop;
      end loop;
      return Item(Start..Finis);
   end Max;
   A : Int_Array := (-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1);
   B : Int_Array := Max(A);
begin
   for I in B'range loop
      Put_Line(Integer'Image(B(I)));
   end loop;
exception
   when Empty_Error =>
      Put_Line("Array being analyzed has no elements.");
end Max_Subarray;
