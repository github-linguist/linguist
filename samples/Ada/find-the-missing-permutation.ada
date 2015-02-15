with Ada.Text_IO;
procedure Missing_Permutations is
   subtype Permutation_Character is Character range 'A' .. 'D';

   Character_Count : constant :=
      1 + Permutation_Character'Pos (Permutation_Character'Last)
        - Permutation_Character'Pos (Permutation_Character'First);

   type Permutation_String is
     array (1 .. Character_Count) of Permutation_Character;

   procedure Put (Item : Permutation_String) is
   begin
      for I in Item'Range loop
         Ada.Text_IO.Put (Item (I));
      end loop;
   end Put;

   Given_Permutations : array (Positive range <>) of Permutation_String :=
     ("ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD",
      "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA",
      "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD",
      "BADC", "BDAC", "CBDA", "DBCA", "DCAB");

   Count     : array (Permutation_Character, 1 .. Character_Count) of Natural
      := (others => (others => 0));
   Max_Count : Positive := 1;

   Missing_Permutation : Permutation_String;
begin
   for I in Given_Permutations'Range loop
      for Pos in 1 .. Character_Count loop
         Count (Given_Permutations (I) (Pos), Pos)   :=
           Count (Given_Permutations (I) (Pos), Pos) + 1;
         if Count (Given_Permutations (I) (Pos), Pos) > Max_Count then
            Max_Count := Count (Given_Permutations (I) (Pos), Pos);
         end if;
      end loop;
   end loop;

   for Char in Permutation_Character loop
      for Pos in 1 .. Character_Count loop
         if Count (Char, Pos) < Max_Count then
            Missing_Permutation (Pos) := Char;
         end if;
      end loop;
   end loop;

   Ada.Text_IO.Put_Line ("Missing Permutation:");
   Put (Missing_Permutation);
end Missing_Permutations;
