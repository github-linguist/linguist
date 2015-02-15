with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

procedure Minesweeper is
   package IO renames Ada.Text_IO;
   package Nat_IO is new IO.Integer_IO (Natural);
   package Nat_RNG is new Ada.Numerics.Discrete_Random (Natural);

   type Stuff is (Empty, Mine);
   type Field is record
      Contents : Stuff   := Empty;
      Opened   : Boolean := False;
      Marked   : Boolean := False;
   end record;
   type Grid is array (Positive range <>, Positive range <>) of Field;

   -- counts how many mines are in the surrounding fields
   function Mines_Nearby (Item : Grid; X, Y : Positive) return Natural is
      Result : Natural := 0;
   begin
      -- left of X:Y
      if X > Item'First (1) then
         -- above X-1:Y
         if Y > Item'First (2) then
            if Item (X - 1, Y - 1).Contents = Mine then
               Result := Result + 1;
            end if;
         end if;
         -- X-1:Y
         if Item (X - 1, Y).Contents = Mine then
            Result := Result + 1;
         end if;
         -- below X-1:Y
         if Y < Item'Last (2) then
            if Item (X - 1, Y + 1).Contents = Mine then
               Result := Result + 1;
            end if;
         end if;
      end if;
      -- above of X:Y
      if Y > Item'First (2) then
         if Item (X, Y - 1).Contents = Mine then
            Result := Result + 1;
         end if;
      end if;
      -- below of X:Y
      if Y < Item'Last (2) then
         if Item (X, Y + 1).Contents = Mine then
            Result := Result + 1;
         end if;
      end if;
      -- right of X:Y
      if X < Item'Last (1) then
         -- above X+1:Y
         if Y > Item'First (2) then
            if Item (X + 1, Y - 1).Contents = Mine then
               Result := Result + 1;
            end if;
         end if;
         -- X+1:Y
         if Item (X + 1, Y).Contents = Mine then
            Result := Result + 1;
         end if;
         -- below X+1:Y
         if Y < Item'Last (2) then
            if Item (X + 1, Y + 1).Contents = Mine then
               Result := Result + 1;
            end if;
         end if;
      end if;
      return Result;
   end Mines_Nearby;

   -- outputs the grid
   procedure Put (Item : Grid) is
      Mines : Natural := 0;
   begin
      IO.Put ("   ");
      for X in Item'Range (1) loop
         Nat_IO.Put (Item => X, Width => 3);
      end loop;
      IO.New_Line;
      IO.Put ("   +");
      for X in Item'Range (1) loop
         IO.Put ("---");
      end loop;
      IO.Put ('+');
      IO.New_Line;
      for Y in Item'Range (2) loop
         Nat_IO.Put (Item => Y, Width => 3);
         IO.Put ('|');
         for X in Item'Range (1) loop
            if Item (X, Y).Opened then
               if Item (X, Y).Contents = Empty then
                  if Item (X, Y).Marked then
                     IO.Put (" - ");
                  else
                     Mines := Mines_Nearby (Item, X, Y);
                     if Mines > 0 then
                        Nat_IO.Put (Item => Mines, Width => 2);
                        IO.Put (' ');
                     else
                        IO.Put ("   ");
                     end if;
                  end if;
               else
                  if Item (X, Y).Marked then
                     IO.Put (" + ");
                  else
                     IO.Put (" X ");
                  end if;
               end if;
            elsif Item (X, Y).Marked then
               IO.Put (" ? ");
            else
               IO.Put (" . ");
            end if;
         end loop;
         IO.Put ('|');
         IO.New_Line;
      end loop;
      IO.Put ("   +");
      for X in Item'Range (1) loop
         IO.Put ("---");
      end loop;
      IO.Put ('+');
      IO.New_Line;
   end Put;

   -- marks a field as possible bomb
   procedure Mark (Item : in out Grid; X, Y : in Positive) is
   begin
      if Item (X, Y).Opened then
         IO.Put_Line ("Field already open!");
      else
         Item (X, Y).Marked := not Item (X, Y).Marked;
      end if;
   end Mark;

   -- clears a field and it's neighbours, if they don't have mines
   procedure Clear
     (Item   : in out Grid;
      X, Y   : in Positive;
      Killed : out Boolean)
   is
      -- clears the neighbours, if they don't have mines
      procedure Clear_Neighbours (The_X, The_Y : Positive) is
      begin
         -- mark current field opened
         Item (The_X, The_Y).Opened := True;
         -- only proceed if neighbours don't have mines
         if Mines_Nearby (Item, The_X, The_Y) = 0 then
            -- left of X:Y
            if The_X > Item'First (1) then
               -- above X-1:Y
               if The_Y > Item'First (2) then
                  if not Item (The_X - 1, The_Y - 1).Opened and
                     not Item (The_X - 1, The_Y - 1).Marked
                  then
                     Clear_Neighbours (The_X - 1, The_Y - 1);
                  end if;
               end if;
               -- X-1:Y
               if not Item (The_X - 1, The_Y).Opened and
                  not Item (The_X - 1, The_Y).Marked
               then
                  Clear_Neighbours (The_X - 1, The_Y);
               end if;
               -- below X-1:Y
               if The_Y < Item'Last (2) then
                  if not Item (The_X - 1, The_Y + 1).Opened and
                     not Item (The_X - 1, The_Y + 1).Marked
                  then
                     Clear_Neighbours (The_X - 1, The_Y + 1);
                  end if;
               end if;
            end if;
            -- above X:Y
            if The_Y > Item'First (2) then
               if not Item (The_X, The_Y - 1).Opened and
                  not Item (The_X, The_Y - 1).Marked
               then
                  Clear_Neighbours (The_X, The_Y - 1);
               end if;
            end if;
            -- below X:Y
            if The_Y < Item'Last (2) then
               if not Item (The_X, The_Y + 1).Opened and
                  not Item (The_X, The_Y + 1).Marked
               then
                  Clear_Neighbours (The_X, The_Y + 1);
               end if;
            end if;
            -- right of X:Y
            if The_X < Item'Last (1) then
               -- above X+1:Y
               if The_Y > Item'First (2) then
                  if not Item (The_X + 1, The_Y - 1).Opened and
                     not Item (The_X + 1, The_Y - 1).Marked
                  then
                     Clear_Neighbours (The_X + 1, The_Y - 1);
                  end if;
               end if;
               -- X+1:Y
               if not Item (The_X + 1, The_Y).Opened and
                  not Item (The_X + 1, The_Y).Marked
               then
                  Clear_Neighbours (The_X + 1, The_Y);
               end if;
               -- below X+1:Y
               if The_Y < Item'Last (2) then
                  if not Item (The_X + 1, The_Y + 1).Opened and
                     not Item (The_X + 1, The_Y + 1).Marked
                  then
                     Clear_Neighbours (The_X + 1, The_Y + 1);
                  end if;
               end if;
            end if;
         end if;
      end Clear_Neighbours;
   begin
      Killed := False;
      -- only clear closed and unmarked fields
      if Item (X, Y).Opened then
         IO.Put_Line ("Field already open!");
      elsif Item (X, Y).Marked then
         IO.Put_Line ("Field already marked!");
      else
         Killed := Item (X, Y).Contents = Mine;
         -- game over if killed, no need to clear
         if not Killed then
            Clear_Neighbours (X, Y);
         end if;
      end if;
   end Clear;

   -- marks all fields as open
   procedure Open_All (Item : in out Grid) is
   begin
      for X in Item'Range (1) loop
         for Y in Item'Range (2) loop
            Item (X, Y).Opened := True;
         end loop;
      end loop;
   end Open_All;

   -- counts the number of marks
   function Count_Marks (Item : Grid) return Natural is
      Result : Natural := 0;
   begin
      for X in Item'Range (1) loop
         for Y in Item'Range (2) loop
            if Item (X, Y).Marked then
               Result := Result + 1;
            end if;
         end loop;
      end loop;
      return Result;
   end Count_Marks;

   -- read and validate user input
   procedure Get_Coordinates
     (Max_X, Max_Y : Positive;
      X, Y         : out Positive;
      Valid        : out Boolean)
   is
   begin
      Valid := False;
      IO.Put ("X: ");
      Nat_IO.Get (X);
      IO.Put ("Y: ");
      Nat_IO.Get (Y);
      Valid := X > 0 and X <= Max_X and Y > 0 and Y <= Max_Y;
   exception
      when Constraint_Error =>
         Valid := False;
   end Get_Coordinates;

   -- randomly place bombs
   procedure Set_Bombs (Item : in out Grid; Max_X, Max_Y, Count : Positive) is
      Generator : Nat_RNG.Generator;
      X, Y      : Positive;
   begin
      Nat_RNG.Reset (Generator);
      for I in 1 .. Count loop
         Placement : loop
            X := Nat_RNG.Random (Generator) mod Max_X + 1;
            Y := Nat_RNG.Random (Generator) mod Max_Y + 1;
            -- redo placement if X:Y already full
            if Item (X, Y).Contents = Empty then
               Item (X, Y).Contents := Mine;
               exit Placement;
            end if;
         end loop Placement;
      end loop;
   end Set_Bombs;

   Width, Height : Positive;

begin
   -- can be dynamically set
   Width  := 6;
   Height := 4;
   declare
      The_Grid : Grid (1 .. Width, 1 .. Height);
      -- 20% bombs
      Bomb_Count         : Positive := Width * Height * 20 / 100;
      Finished           : Boolean  := False;
      Action             : Character;
      Chosen_X, Chosen_Y : Positive;
      Valid_Entry        : Boolean;
   begin
      IO.Put ("Nr. Bombs: ");
      Nat_IO.Put (Item => Bomb_Count, Width => 0);
      IO.New_Line;
      Set_Bombs
        (Item  => The_Grid,
         Max_X => Width,
         Max_Y => Height,
         Count => Bomb_Count);
      while not Finished and Count_Marks (The_Grid) /= Bomb_Count loop
         Put (The_Grid);
         IO.Put ("Input (c/m/r): ");
         IO.Get (Action);
         case Action is
            when 'c' | 'C' =>
               Get_Coordinates
                 (Max_X => Width,
                  Max_Y => Height,
                  X     => Chosen_X,
                  Y     => Chosen_Y,
                  Valid => Valid_Entry);
               if Valid_Entry then
                  Clear
                    (Item   => The_Grid,
                     X      => Chosen_X,
                     Y      => Chosen_Y,
                     Killed => Finished);
                  if Finished then
                     IO.Put_Line ("You stepped on a mine!");
                  end if;
               else
                  IO.Put_Line ("Invalid input, retry!");
               end if;
            when 'm' | 'M' =>
               Get_Coordinates
                 (Max_X => Width,
                  Max_Y => Height,
                  X     => Chosen_X,
                  Y     => Chosen_Y,
                  Valid => Valid_Entry);
               if Valid_Entry then
                  Mark (Item => The_Grid, X => Chosen_X, Y => Chosen_Y);
               else
                  IO.Put_Line ("Invalid input, retry!");
               end if;
            when 'r' | 'R' =>
               Finished := True;
            when others =>
               IO.Put_Line ("Invalid input, retry!");
         end case;
      end loop;
      Open_All (The_Grid);
      IO.Put_Line
        ("Solution: (+ = correctly marked, - = incorrectly marked)");
      Put (The_Grid);
   end;
end Minesweeper;
