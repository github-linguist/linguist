with Ada.Text_IO;  use Ada.Text_IO;

procedure Cellular_Automata is
   type Petri_Dish is array (Positive range <>) of Boolean;

   procedure Step (Culture : in out Petri_Dish) is
      Left  : Boolean := False;
      This  : Boolean;
      Right : Boolean;
   begin
      for Index in Culture'First..Culture'Last - 1 loop
         Right := Culture (Index + 1);
         This  := Culture (Index);
         Culture (Index) := (This and (Left xor Right)) or (not This and Left and Right);
         Left := This;
      end loop;
      Culture (Culture'Last) := Culture (Culture'Last) and not Left;
   end Step;

   procedure Put (Culture : Petri_Dish) is
   begin
      for Index in Culture'Range loop
         if Culture (Index) then
            Put ('#');
         else
            Put ('_');
         end if;
      end loop;
   end Put;

   Culture : Petri_Dish :=
      (  False, True, True,  True, False, True,  True, False, True, False, True,
         False, True, False, True, False, False, True, False, False
      );
begin
   for Generation in 0..9 loop
      Put ("Generation" & Integer'Image (Generation) & ' ');
      Put (Culture);
      New_Line;
      Step (Culture);
   end loop;
end Cellular_Automata;
