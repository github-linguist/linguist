with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Sum_Of_Squares is
   type Float_Array is array (Integer range <>) of Float;

   function Sum_Of_Squares (X : Float_Array) return Float is
      Sum : Float := 0.0;
   begin
      for I in X'Range loop
         Sum := Sum + X (I) ** 2;
      end loop;
      return Sum;
   end Sum_Of_Squares;

begin
   Put_Line (Float'Image (Sum_Of_Squares ((1..0 => 1.0)))); -- Empty array
   Put_Line (Float'Image (Sum_Of_Squares ((3.0, 1.0, 4.0, 1.0, 5.0, 9.0))));
end Test_Sum_Of_Squares;
