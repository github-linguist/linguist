with Ada.Text_IO;

procedure Langtons_Ant is

   Size: constant Positive := 100; -- change this to extend the playground

   subtype Step is Integer range -1 .. +1;

   procedure Right(N, W: in out Step) is
      Tmp: Step := W;
   begin
      W := - N;
      N := Tmp;
   end Right;

   procedure Left(N, W: in out Step) is
   begin
      for I in 1 .. 3 loop
         Right(N, W);
      end loop;
   end Left;

   Color_Character: array(Boolean) of Character :=
     (False => ' ', True => '#');

   Is_Black: array (1 .. Size, 1 .. Size) of Boolean :=
     (others => (others => False)); -- initially, the world is white;

   Ant_X, Ant_Y: Natural := Size/2; -- Position of Ant;
   Ant_North: Step := 1; Ant_West: Step := 0; -- initially, Ant looks northward

   Iteration: Positive := 1;

begin
   loop -- iterate the loop until an exception is raised
      if Is_Black(Ant_X, Ant_Y) then
         Left(Ant_North, Ant_West);
      else
         Right(Ant_North, Ant_West);
      end if;
      Is_Black(Ant_X, Ant_Y) := not Is_Black(Ant_X, Ant_Y);
      Ant_X := Ant_X - Ant_North; -- this may raise an exception
      Ant_Y := Ant_Y - Ant_West;  -- this may raise an exception
      Iteration := Iteration + 1;
    end loop;

exception
   when Constraint_Error => -- Ant has left its playground ... now output
      for X in 1 .. Size loop
         for Y in 1 .. Size loop
            Ada.Text_IO.Put(Color_Character(Is_Black(X, Y)));
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.Put_Line("# Iteration:" & Integer'Image(Iteration));
end Langtons_Ant;
