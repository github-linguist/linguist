with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Forest_Fire is
   type Cell is (Empty, Tree, Fire);
   type Board is array (Positive range <>, Positive range <>) of Cell;
   procedure Step (S : in out Board; P, F : Float; Dice : Generator) is
      function "+" (Left : Boolean; Right : Cell) return Boolean is
      begin
         return Left or else Right = Fire;
      end "+";
      function "+" (Left, Right : Cell) return Boolean is
      begin
         return Left = Fire or else Right = Fire;
      end "+";
      Above : array (S'Range (2)) of Cell := (others => Empty);
      Left_Up, Up, Left : Cell;
   begin
      for Row in S'First (1) + 1..S'Last (1) - 1 loop
         Left_Up := Empty;
         Up      := Empty;
         Left    := Empty;
         for Column in S'First (2) + 1..S'Last (2) - 1 loop
            Left_Up := Up;
            Up      := Above (Column);
            Above (Column) := S (Row, Column);
            case S (Row, Column) is
               when Empty =>
                  if Random (Dice) < P then
                     S (Row, Column) := Tree;
                  end if;
               when Tree =>
                  if Left_Up                 + Up                  +      Above (Column + 1) +
                     Left                    + S (Row,     Column) + S (Row,     Column + 1) +
                     S (Row + 1, Column - 1) + S (Row + 1, Column) + S (Row + 1, Column + 1)
                  or else Random (Dice) < F then
                     S (Row, Column) := Fire;
                  end if;
               when Fire =>
                  S (Row, Column) := Empty;
            end case;
            Left := Above (Column);
         end loop;
      end loop;
   end Step;
   procedure Put (S : Board) is
   begin
      for Row in S'First (1) + 1..S'Last (1) - 1 loop
         for Column in S'First (2) + 1..S'Last (2) - 1 loop
            case S (Row, Column) is
               when Empty => Put (' ');
               when Tree  => Put ('Y');
               when Fire  => Put ('#');
            end case;
         end loop;
         New_Line;
      end loop;
   end Put;

   Dice   : Generator;
   Forest : Board := (1..10 => (1..40 => Empty));
begin
   Reset (Dice);
   for I in 1..10 loop
      Step (Forest, 0.3, 0.1, Dice);
      Put_Line ("-------------" & Integer'Image (I) & " -------------");
      Put (Forest);
   end loop;
end Forest_Fire;
