with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Wireworld is
   type Cell is (' ', 'H', 't', '.');
   type Board is array (Positive range <>, Positive range <>) of Cell;
      -- Perform one transition of the cellular automation
   procedure Wireworld (State : in out Board) is
      function "abs" (Left : Cell) return Natural is
      begin
         if Left = 'H' then
            return 1;
         else
            return 0;
         end if;
      end "abs";
      Above   : array (State'Range (2)) of Cell := (others => ' ');
      Left    : Cell := ' ';
      Current : Cell;
   begin
      for I in State'First (1) + 1..State'Last (1) - 1 loop
         for J in State'First (2) + 1..State'Last (2) - 1 loop
            Current := State (I, J);
            case Current is
               when ' ' =>
                  null;
               when 'H' =>
                  State (I, J) := 't';
               when 't' =>
                  State (I, J) := '.';
               when '.' =>
                  if abs Above (       J - 1) + abs Above (       J) + abs Above (       J + 1) +
                     abs Left                                        + abs State (I,     J + 1) +
                     abs State (I + 1, J - 1) + abs State (I + 1, J) + abs State (I + 1, J + 1)
                  in 1..2 then
                     State (I, J) := 'H';
                  else
                     State (I, J) := '.';
                  end if;
            end case;
            Above (J - 1) := Left;
            Left := Current;
         end loop;
      end loop;
   end Wireworld;
      -- Print state of the automation
   procedure Put (State : Board) is
   begin
      for I in State'First (1) + 1..State'Last (1) - 1 loop
         for J in State'First (2) + 1..State'Last (2) - 1 loop
            case State (I, J) is
               when ' ' => Put (' ');
               when 'H' => Put ('H');
               when 't' => Put ('t');
               when '.' => Put ('.');
            end case;
         end loop;
         New_Line;
      end loop;
   end Put;
   Oscillator : Board := ("         ", "  tH     ", " .  .... ", "  ..     ", "         ");
begin
   for Step in 0..9 loop
      Put_Line ("Step" & Integer'Image (Step) & " ---------"); Put (Oscillator);
      Wireworld (Oscillator);
   end loop;
end Test_Wireworld;
