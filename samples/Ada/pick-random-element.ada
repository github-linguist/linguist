with Ada.Text_IO, Ada.Numerics.Float_Random;

procedure Pick_Random_Element is

   package Rnd renames Ada.Numerics.Float_Random;
   Gen: Rnd.Generator; -- used globally

   type Char_Arr is array (Natural range <>) of Character;

   function Pick_Random(A: Char_Arr) return Character is
      -- Chooses one of the characters of A (uniformly distributed)
   begin
      return A(A'First + Natural(Rnd.Random(Gen) * Float(A'Last)));
   end Pick_Random;

   Vowels    : Char_Arr := ('a', 'e', 'i', 'o', 'u');
   Consonants: Char_Arr := ('t', 'n', 's', 'h', 'r', 'd', 'l');
   Specials  : Char_Arr := (',', '.', '?', '!');

begin
   Rnd.Reset(Gen);
   for J in 1 .. 3 loop
      for I in 1 .. 10 loop
         Ada.Text_IO.Put(Pick_Random(Consonants));
         Ada.Text_IO.Put(Pick_Random(Vowels));
      end loop;
      Ada.Text_IO.Put(Pick_Random(Specials) & " ");
   end loop;
   Ada.Text_IO.New_Line;
end Pick_Random_Element;
