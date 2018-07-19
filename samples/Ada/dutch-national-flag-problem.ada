with Ada.Text_IO, Ada.Numerics.Discrete_Random, Ada.Command_Line;

procedure Dutch_National_Flag is

   type Colour_Type is (Red, White, Blue);

   Number: Positive range 2 .. Positive'Last :=
     Positive'Value(Ada.Command_Line.Argument(1));
   -- no sorting if the Number of balls is less than 2

   type Balls is array(1 .. Number) of Colour_Type;

   function Is_Sorted(B: Balls) return Boolean is
      -- checks if balls are in order
   begin
      for I in Balls'First .. Balls'Last-1 loop
         if B(I) > B(I+1) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Sorted;

   function Random_Balls return Balls is
      -- generates an array of random balls, ensuring they are not in order
      package Random_Colour is new Ada.Numerics.Discrete_Random(Colour_Type);
      Gen: Random_Colour.Generator;
      B: Balls;
   begin
      Random_Colour.Reset(Gen);
      loop
         for I in Balls'Range loop
            B(I) := Random_Colour.Random(Gen);
         end loop;
         exit when (not Is_Sorted(B));
         -- ... ensuring they are not in order
      end loop;
      return B;
   end Random_Balls;

   procedure Print(Message: String; B: Balls) is
   begin
      Ada.Text_IO.Put(Message);
      for I in B'Range loop
         Ada.Text_IO.Put(Colour_Type'Image(B(I)));
         if I < B'Last then
            Ada.Text_IO.Put(", ");
         else
            Ada.Text_IO.New_Line;
         end if;
      end loop;
   end Print;

   procedure Sort(Bls: in out Balls) is
      -- sort Bls in O(1) time

      Cnt: array(Colour_Type) of Natural := (Red => 0, White => 0, Blue => 0);
      Col: Colour_Type;

      procedure Move_Colour_To_Top(Bls: in out Balls;
                                   Colour: Colour_Type;
                                   Start: Positive;
                                   Count: Natural) is
         This: Positive := Start;
         Tmp: Colour_Type;
      begin
         for N in Start .. Start+Count-1 loop
            while Bls(This) /= Colour loop
               This := This + 1;
            end loop; -- This is the first index >= N with B(This) = Colour
            Tmp := Bls(N); Bls(N) := Bls(This); Bls(This) := Tmp; -- swap
            This := This + 1;
         end loop;
      end  Move_Colour_To_Top;

   begin
      for Ball in Balls'Range loop
         -- count how often each colour is found
         Col := Bls(Ball);
         Cnt(Col) := Cnt(Col) + 1;
      end loop;
      Move_Colour_To_Top(Bls, Red,   Start => 1,          Count => Cnt(Red));
      Move_Colour_To_Top(Bls, White, Start => 1+Cnt(Red), Count => Cnt(White));
      -- all the remaining balls are blue
   end Sort;

   A: Balls := Random_Balls;

begin
   Print("Original Order: ", A);

   pragma Assert(not Is_Sorted(A));   -- Check if A is unsorted

   Sort(A); -- A = ((Red**Cnt(Red)= & (White**Cnt(White)) & (Blue**Cnt(Blue)))

   pragma Assert(Is_Sorted(A));   -- Check if A is actually sorted

   Print("After Sorting:  ", A);
end Dutch_National_Flag;
