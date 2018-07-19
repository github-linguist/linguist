with Ada.Text_IO;  use Ada.Text_IO;
procedure Order is

   type IntArray is array (Positive range <>) of Integer;
   List1 : IntArray := (1, 2, 3, 4, 5);
   List2 : IntArray := (1, 2, 1, 5, 2, 2);
   List3 : IntArray := (1, 2, 1, 5, 2);
   List4 : IntArray := (1, 2, 1, 5, 2);

   type Animal is (Rat, Cat, Elephant);
   type AnimalArray is array (Positive range <>) of Animal;
   List5 : AnimalArray := (Cat, Elephant, Rat, Cat);
   List6 : AnimalArray := (Cat, Elephant, Rat);
   List7 : AnimalArray := (Cat, Cat, Elephant);

begin
   Put_Line (Boolean'Image (List1 > List2)); --  True
   Put_Line (Boolean'Image (List2 > List3)); --  True
   Put_Line (Boolean'Image (List3 > List4)); --  False, equal
   Put_Line (Boolean'Image (List5 > List6)); --  True
   Put_Line (Boolean'Image (List6 > List7)); --  True
end Order;
