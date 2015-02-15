with Ada.Text_IO, Ada.Numerics.Discrete_Random;
  -- can play human-human, human-computer, computer-human or computer-computer
  -- the computer isn't very clever: it just chooses a legal random move

procedure Tic_Tac_Toe is

   type The_Range is range 1 .. 3;
   type Board_Type is array (The_Range, The_Range) of Character;

   package Rand is new Ada.Numerics.Discrete_Random(The_Range);
   Gen: Rand.Generator; -- required for the random moves

   procedure Show_Board(Board: Board_Type) is
      use Ada.Text_IO;
   begin
      for Row in The_Range loop
         for Column in The_Range loop
            Put(Board(Row, Column));
         end loop;
         Put_Line("");
      end loop;
      Put_Line("");
   end Show_Board;

   function Find_Winner(Board: Board_Type) return Character is
      -- if 'x' or 'o' wins, it returns that, else it returns ' '

      function Three_Equal(A,B,C: Character) return Boolean is
      begin
         return (A=B) and (A=C);
      end Three_Equal;

   begin -- Find_Winner
      for I in The_Range loop
         if    Three_Equal(Board(I,1), Board(I,2), Board(I,3)) then
            return Board(I,1);
         elsif  Three_Equal(Board(1,I), Board(2,I), Board(3,I)) then
            return Board(1,I);
         end if;
      end loop;
      if Three_Equal(Board(1,1), Board(2,2), Board (3,3)) or
         Three_Equal(Board(3,1), Board(2,2), Board (1,3)) then
         return Board(2,2);
      end if;
      return ' ';
   end Find_Winner;

   procedure Do_Move(Board: in out Board_Type;
                     New_Char: Character; Computer_Move: Boolean) is
      Done: Boolean := False;
      C: Character;
      use Ada.Text_IO;

      procedure Do_C_Move(Board: in out Board_Type; New_Char: Character) is
         Found: Boolean := False;
         X,Y: The_Range;
      begin
         while not Found loop
            X := Rand.Random(Gen);
            Y := Rand.Random(Gen);
            if (Board(X,Y) /= 'x') and  (Board(X,Y) /= 'o') then
               Found := True;
               Board(X,Y) := New_Char;
            end if;
         end loop;
      end Do_C_Move;

   begin
      if Computer_Move then
         Do_C_Move(Board, New_Char);
      else -- read move;
         Put_Line("Choose your move, " & New_Char);
         while not Done loop
            Get(C);
            for Row in The_Range loop
               for Col in The_Range loop
                  if Board(Row, Col) = C then
                     Board(Row, Col) := New_Char;
                     Done := True;
                  end if;
               end loop;
            end loop;
         end loop;
      end if;
   end Do_Move;

   The_Board : Board_Type := (('1','2','3'), ('4','5','6'), ('7','8','9'));
   Cnt_Moves: Natural := 0;
   Players: array(0 .. 1) of Character := ('x', 'o'); -- 'x' begins
   C_Player: array(0 .. 1) of Boolean := (False, False);
   Reply: Character;

begin -- Tic_Tac_Toe

   -- firstly, ask whether the computer shall take over either player
   for I in Players'Range loop
      Ada.Text_IO.Put_Line("Shall " & Players(I) &
                             " be run by the computer? (y=yes)");
      Ada.Text_IO.Get(Reply);
      if Reply='y' or Reply='Y' then
         C_Player(I) := True;
         Ada.Text_IO.Put_Line("Yes!");
      else
         Ada.Text_IO.Put_Line("No!");
      end if;
   end loop;
   Rand.Reset(Gen); -- to initalize the random generator

   -- now run the game
   while (Find_Winner(The_Board) = ' ') and (Cnt_Moves < 9) loop
      Show_Board(The_Board);
      Do_Move(The_Board, Players(Cnt_Moves mod 2), C_Player(Cnt_Moves mod 2));
      Cnt_Moves := Cnt_Moves + 1;
   end loop;
   Ada.Text_IO.Put_Line("This is the end!");

   -- finally, output the outcome
   Show_Board (The_Board);
   if Find_Winner(The_Board) = ' ' then
      Ada.Text_IO.Put_Line("Draw");
   else
      Ada.Text_IO.Put_Line("The winner is: " & Find_Winner(The_Board));
   end if;
end Tic_Tac_Toe;
