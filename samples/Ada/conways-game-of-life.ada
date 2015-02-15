with Ada.Text_IO;  use Ada.Text_IO;

procedure Life is
   type Cell is (O, X); -- Two states of a cell
      -- Computation of neighborhood
   function "+" (L, R : Cell) return Integer is
   begin
      case L is
         when O =>
            case R is
               when O => return 0;
               when X => return 1;
            end case;
         when X =>
            case R is
               when O => return 1;
               when X => return 2;
            end case;
      end case;
   end "+";
   function "+" (L : Integer; R : Cell) return Integer is
   begin
      case R is
         when O => return L;
         when X => return L + 1;
      end case;
   end "+";
      -- A colony of cells. The borders are dire and unhabited
   type Petri_Dish is array (Positive range <>, Positive range <>) of Cell;

   procedure Step (Culture : in out Petri_Dish) is
      Above : array (Culture'Range (2)) of Cell := (others => O);
      Left  : Cell;
      This  : Cell;
   begin
      for I in Culture'First (1) + 1 .. Culture'Last (1) - 1 loop
         Left := O;
         for J in Culture'First (2) + 1 .. Culture'Last (2) - 1 loop
            case Above        (J-1) + Above        (J) + Above        (J+1) +
                 Left                                  + Culture (I,   J+1) +
                 Culture (I+1, J-1) + Culture (I+1, J) + Culture (I+1, J+1) is
               when 2 =>     -- Survives if alive
                  This := Culture (I, J);
               when 3 =>     -- Survives or else multiplies
                  This := X;
               when others => -- Dies
                  This := O;
            end case;
            Above (J-1) := Left;
            Left        := Culture (I, J);
            Culture (I, J) := This;
         end loop;
         Above (Above'Last - 1) := Left;
      end loop;
   end Step;

   procedure Put (Culture : Petri_Dish) is
   begin
      for I in Culture'Range loop
         for J in Culture'Range loop
            case Culture (I, J) is
               when O => Put (' ');
               when X => Put ('#');
            end case;
         end loop;
         New_Line;
      end loop;
   end Put;

   Blinker : Petri_Dish := (2..4 =>(O,O,X,O,O), 1|5 =>(O,O,O,O,O));
   Glider  : Petri_Dish :=
             (  (O,O,O,O,O,O,O,O,O,O,O),
                (O,O,X,O,O,O,O,O,O,O,O),
                (O,O,O,X,O,O,O,O,O,O,O),
                (O,X,X,X,O,O,O,O,O,O,O),
                (O,O,O,O,O,O,O,O,O,O,O),
                (O,O,O,O,O,O,O,O,O,O,O)
             );
begin
   for Generation in 1..3 loop
      Put_Line ("Blinker" & Integer'Image (Generation));
      Put (Blinker);
      Step (Blinker);
   end loop;
   for Generation in 1..5 loop
      Put_Line ("Glider" & Integer'Image (Generation));
      Put (Glider);
      Step (Glider);
   end loop;
end Life;
