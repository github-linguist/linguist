with Ada.Text_IO;

procedure Maze_Solver is

   X_Size: constant Natural := 45;
   Y_Size: constant Natural := 17;

   subtype X_Range is Natural range 1 .. X_Size;
   subtype Y_Range is Natural range 1 .. Y_Size;

   East:  constant X_Range := 2;
   South: constant Y_Range := 1;

   X_Start: constant X_Range  := 3; -- start at the upper left
   Y_Start: constant Y_Range  := 1;
   X_Finish: constant X_Range := X_Size-East; -- go to the lower right
   Y_Finish: constant Y_Range := Y_Size;

   type Maze_Type is array (Y_Range) of String(X_Range);

   function Solved(X: X_Range; Y: Y_Range) return Boolean is
   begin
      return (X = X_Finish) and (Y = Y_Finish);
   end Solved;

   procedure Output_Maze(M: Maze_Type; Message: String := "") is
   begin
      if Message /= "" then
         Ada.Text_IO.Put_Line(Message);
      end if;
      for I in M'Range loop
         Ada.Text_IO.Put_Line(M(I));
      end loop;
   end Output_Maze;

   procedure Search(M: in out Maze_Type; X: X_Range; Y:Y_Range) is
   begin
      M(Y)(X) := '*';
      if Solved(X, Y) then
         Output_Maze(M, "Solution found!");
      else
         if Integer(Y)-South >= 1 and then M(Y-South)(X) = ' ' then
            Search(M, X, Y-South);
         end if;
         if Integer(Y)+South <= Y_Size and then M(Y+South)(X) = ' ' then
            Search(M, X, Y+South);
         end if;
         if Integer(X)-East >= 1 and then M(Y)(X-East) = ' ' then
            Search(M, X-East, Y);
         end if;
         if Integer(Y)+East <= Y_Size and then M(Y)(X+East) = ' ' then
            Search(M, X+East, Y);
         end if;
      end if;
      M(Y)(X) := ' ';
   end Search;

   Maze: Maze_Type;
   X: X_Range := X_Start;
   Y: Y_Range := Y_Start;

begin
   for I in 1 .. Y_Size loop
      Maze(I) := Ada.Text_IO.Get_Line;
   end loop;
   Maze(Y_Start)(X_Start)   := ' '; -- Start from
   Maze(Y_Finish)(X_Finish) := ' '; -- Go_To
   Output_Maze(Maze, "The Maze:");
   Ada.Text_IO.New_Line;

   Search(Maze, X, Y) ; -- Will output *all* Solutions.
                        -- If there is no output, there is no solution.
end Maze_Solver;
