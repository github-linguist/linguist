with Ada.Text_Io;
with Ada.Float_Text_Io;
with Ada.Integer_Text_Io;

procedure Two_Dimensional_Arrays is
   type Matrix_Type is array(Positive range <>, Positive range <>) of Float;
   Dim_1 : Positive;
   Dim_2 : Positive;
begin
   Ada.Integer_Text_Io.Get(Item => Dim_1);
   Ada.Integer_Text_Io.Get(Item => Dim_2);
   -- Create an inner block with the correctly sized array
   declare
      Matrix : Matrix_Type(1..Dim_1, 1..Dim_2);
   begin
      Matrix(1, Dim_2) := 3.14159;
      Ada.Float_Text_Io.Put(Item => Matrix(1, Dim_2), Fore => 1, Aft => 5, Exp => 0);
      Ada.Text_Io.New_Line;
   end;
   -- The variable Matrix is popped off the stack automatically
end Two_Dimensional_Arrays;
