with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Numerics.Discrete_Random;

procedure Bulls_Player is

   -- package for In-/Output of natural numbers
   package Nat_IO is new Ada.Text_IO.Integer_IO (Natural);

   -- for comparing length of the vectors
   use type Ada.Containers.Count_Type;

   -- number of digits
   Guessing_Length : constant := 4;

   -- digit has to be from 1 to 9
   type Digit is range 1 .. 9;
   -- a sequence has specified length of digits
   type Sequence is array (1 .. Guessing_Length) of Digit;

   -- data structure to store the possible answers
   package Sequence_Vectors is new Ada.Containers.Vectors
     (Element_Type => Sequence,
      Index_Type   => Positive);

   -- check if sequence contains each digit only once
   function Is_Valid (S : Sequence) return Boolean is
      Appeared : array (Digit) of Boolean := (others => False);
   begin
      for I in S'Range loop
         if Appeared (S (I)) then
            return False;
         end if;
         Appeared (S (I))  := True;
      end loop;
      return True;
   end Is_Valid;

   -- calculate all possible sequences and store them in the vector
   procedure Fill_Pool (Pool : in out Sequence_Vectors.Vector) is
      Finished : exception;
      -- count the sequence up by one
      function Next (S : Sequence) return Sequence is
         Result : Sequence := S;
         Index  : Positive := S'Last;
      begin
         loop
         -- overflow at a position causes next position to increase
            if Result (Index) = Digit'Last then
               Result (Index) := Digit'First;
               -- overflow at maximum position
               -- we have processed all possible values
               if Index = Result'First then
                  raise Finished;
               end if;
               Index := Index - 1;
            else
               Result (Index) := Result (Index) + 1;
               return Result;
            end if;
         end loop;
      end Next;
      X        : Sequence := (others => 1);
   begin
      loop
      -- append all valid values
         if Is_Valid (X) then
            Pool.Append (X);
         end if;
         X := Next (X);
      end loop;
   exception
      when Finished =>
         -- the exception tells us that we have added all possible values
         -- simply return and do nothing.
         null;
   end Fill_Pool;

   -- generate a random index from the pool
   function Random_Index (Pool : Sequence_Vectors.Vector) return Positive is
      subtype Possible_Indexes is Positive range
        Pool.First_Index .. Pool.Last_Index;
      package Index_Random is new Ada.Numerics.Discrete_Random
        (Possible_Indexes);
      Index_Gen : Index_Random.Generator;
   begin
      Index_Random.Reset (Index_Gen);
      return Index_Random.Random (Index_Gen);
   end Random_Index;

   -- get the answer from the player, simple validity tests
   procedure Get_Answer (S : Sequence; Bulls, Cows : out Natural) is
      Valid : Boolean := False;
   begin
      Bulls := 0;
      Cows  := 0;
      while not Valid loop
         -- output the sequence
         Ada.Text_IO.Put ("How is the score for:");
         for I in S'Range loop
            Ada.Text_IO.Put (Digit'Image (S (I)));
         end loop;
         Ada.Text_IO.New_Line;
         begin
            Ada.Text_IO.Put ("Bulls:");
            Nat_IO.Get (Bulls);
            Ada.Text_IO.Put ("Cows:");
            Nat_IO.Get (Cows);
            if Bulls + Cows <= Guessing_Length then
               Valid := True;
            else
               Ada.Text_IO.Put_Line ("Invalid answer, try again.");
            end if;
         exception
            when others =>
               null;
         end;
      end loop;
   end Get_Answer;

   -- remove all sequences that wouldn't give an equivalent score
   procedure Strip
     (V           : in out Sequence_Vectors.Vector;
      S           : Sequence;
      Bulls, Cows : Natural)
   is
      function Has_To_Be_Removed (Position : Positive) return Boolean is
         Testant    : constant Sequence := V.Element (Position);
         Bull_Score : Natural           := 0;
         Cows_Score : Natural := 0;
      begin
         for I in Testant'Range loop
            for J in S'Range loop
               if Testant (I) = S (J) then
                  -- same digit at same position: Bull!
                  if I = J then
                     Bull_Score := Bull_Score + 1;
                  else
                     Cow_Score := Cow_Score + 1;
                  end if;
               end if;
            end loop;
         end loop;
         return Cow_Score /= Cows or else Bull_Score /= Bulls;
      end Has_To_Be_Removed;
   begin
      for Index in reverse V.First_Index .. V.Last_Index loop
         if Has_To_Be_Removed (Index) then
            V.Delete (Index);
         end if;
      end loop;
   end Strip;

   -- main routine
   procedure Solve is
      All_Sequences : Sequence_Vectors.Vector;
      Test_Index    : Positive;
      Test_Sequence : Sequence;
      Bulls, Cows   : Natural;
   begin
      -- generate all possible sequences
      Fill_Pool (All_Sequences);
      loop
      -- pick at random
         Test_Index    := Random_Index (All_Sequences);
         Test_Sequence := All_Sequences.Element (Test_Index);
         -- ask player
         Get_Answer (Test_Sequence, Bulls, Cows);
         -- hooray, we have it!
         exit when Bulls = 4;
         All_Sequences.Delete (Test_Index);
         Strip (All_Sequences, Test_Sequence, Bulls, Cows);
         exit when All_Sequences.Length <= 1;
      end loop;
      if All_Sequences.Length = 0 then
         -- oops, shouldn't happen
         Ada.Text_IO.Put_Line
           ("I give up, there has to be a bug in" &
            "your scoring or in my algorithm.");
      else
         if All_Sequences.Length = 1 then
            Ada.Text_IO.Put ("The sequence you thought has to be:");
            Test_Sequence := All_Sequences.First_Element;
         else
            Ada.Text_IO.Put ("The sequence you thought of was:");
         end if;
         for I in Test_Sequence'Range loop
            Ada.Text_IO.Put (Digit'Image (Test_Sequence (I)));
         end loop;
      end if;
   end Solve;

begin
   -- output blah blah
   Ada.Text_IO.Put_Line ("Bulls and Cows, Your turn!");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line
     ("Think of a sequence of" &
      Integer'Image (Guessing_Length) &
      " different digits.");
   Ada.Text_IO.Put_Line ("I will try to guess it. For each correctly placed");
   Ada.Text_IO.Put_Line ("digit I score 1 Bull. For each digit that is on");
   Ada.Text_IO.Put_Line ("the wrong place I score 1 Cow. After each guess");
   Ada.Text_IO.Put_Line ("you tell me my score.");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Let's start.");
   Ada.Text_IO.New_Line;
   -- solve the puzzle
   Solve;
end Bulls_Player;
