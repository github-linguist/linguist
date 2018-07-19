with Ada.Text_IO; with Ada.Numerics.Float_Random;

procedure Rock_Paper_Scissors is

   package Rand renames Ada.Numerics.Float_Random;
   Gen: Rand.Generator;

   type Choice is (Rock, Paper, Scissors);

   Cnt: array (Choice) of Natural := (1, 1, 1);
     -- for the initialization: pretend that each of Rock, Paper,
     -- and Scissors, has been played once by the human
     -- else the first computer choice would be deterministic

   function Computer_Choice return Choice is
      Random_Number: Natural :=
        Integer(Rand.Random(Gen)
          * (Float(Cnt(Rock)) + Float(Cnt(Paper)) + Float(Cnt(Scissors))));
   begin
      if Random_Number < Cnt(Rock) then
         -- guess the human will choose Rock
         return Paper;
      elsif Random_Number - Cnt(Rock) < Cnt(Paper) then
         -- guess the human will choose Paper
         return Scissors;
      else -- guess the human will choose Scissors
         return Rock;
      end if;
   end Computer_Choice;

   Finish_The_Game: exception;

   function Human_Choice return Choice is
      Done: Boolean := False;
      T: constant String
        := "enter ""r"" for Rock, ""p"" for Paper, or ""s"" for Scissors""!";
      U: constant String
        := "or enter ""q"" to Quit the game";
      Result: Choice;
   begin
      Ada.Text_IO.Put_Line(T);
      Ada.Text_IO.Put_Line(U);
      while not Done loop
         Done := True;
         declare
            S: String := Ada.Text_IO.Get_Line;
         begin
            if S="r" or S="R" then
               Result := Rock;
            elsif S="p" or S = "P" then
               Result := Paper;
            elsif S="s" or S="S" then
               Result := Scissors;
            elsif S="q" or S="Q" then
               raise Finish_The_Game;
            else
               Done := False;
            end if;
         end;
      end loop;
      return Result;
   end Human_Choice;

   type Result is (Human_Wins, Draw, Computer_Wins);

   function "<" (X, Y: Choice) return Boolean is
      -- X < Y if X looses against Y
   begin
      case X is
         when Rock => return  (Y = Paper);
         when Paper => return (Y = Scissors);
         when Scissors => return (Y = Rock);
      end case;
   end "<";

   Score: array(Result) of Natural := (0, 0, 0);

   C,H: Choice;

   Res: Result;

begin
   -- play the game
   loop
      C := Computer_Choice;  -- the computer makes its choice first
      H := Human_Choice;     -- now ask the player for his/her choice
      Cnt(H) := Cnt(H) + 1;  -- update the counts for the AI
      if C < H then
         Res := Human_Wins;
      elsif H < C then
         Res := Computer_Wins;
      else
         Res := Draw;
      end if;
      Ada.Text_IO.Put_Line("COMPUTER'S CHOICE: " & Choice'Image(C)
                             & "       RESULT: " & Result'Image(Res));
      Ada.Text_IO.New_Line;
      Score(Res) := Score(Res) + 1;
   end loop;

exception
   when Finish_The_Game =>
      Ada.Text_IO.New_Line;
      for R in Score'Range loop
         Ada.Text_IO.Put_Line(Result'Image(R) & Natural'Image(Score(R)));
      end loop;
end Rock_Paper_Scissors;
