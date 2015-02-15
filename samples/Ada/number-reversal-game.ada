with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Numerics.Discrete_Random;

procedure NumberReverse is

   subtype RandRange is Integer range 1..9;
   type NumArrayType is array (Integer range 1..9) of Integer;

   package RandNumbers is new Ada.Numerics.Discrete_Random(RandRange);
   use RandNumbers;

   G : Generator;

   procedure FillArray (A : in out NumArrayType) is
      Temp : RandRange;
   begin
      A := (others => 0);
      for I in 1..9 loop
         Temp := Random(G);
         while A(Temp) /= 0 loop
            Temp := Random(G);
         end loop;
         A(Temp) := I;
      end loop;
   end FillArray;

   procedure Put(A : in NumArrayType) is
   begin
      for I in 1..9 loop
         Put(A(I), 0);
         Put(" ");
      end loop;
   end Put;

   procedure Prompt (Index : out Integer) is
   begin
      New_Line;
      Put("How many numbers would you like to reverse: ");
      Get(Index);
   end Prompt;

   procedure ReverseArray(Arr : in out NumArrayType;
                          Index : in Integer) is
      Temp : RandRange;
   begin
      for I in 1..Index/2 loop
         Temp := Arr(I);
         Arr(I) := Arr(Index + 1 - I);
         Arr(Index + 1 - I) := Temp;
      end loop;
   end ReverseArray;

   Sorted : constant NumArrayType := (1,2,3,4,5,6,7,8,9);
   Arr    : NumArrayType;
   Index  : Integer;
   Count  : Integer := 0;
begin
   Reset(G);
   loop
      FillArray(Arr);
      exit when Sorted /= Arr;
   end loop;
   loop
      Put(Arr);
      Prompt(Index);
      Count := Count + 1;
      ReverseArray(Arr, Index);
      exit when Sorted = Arr;
   end loop;
   Put(Arr);
   New_Line;
   Put("Congratulations! You win. It took " &
         Integer'Image(Count) & " tries.");
end NumberReverse;
