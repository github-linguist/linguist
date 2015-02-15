with Ada.Text_IO;

procedure Best_Shuffle is

   function Best_Shuffle(S: String) return String is
      T: String(S'Range) := S;
      Tmp: Character;
   begin
      for I in S'Range loop
         for J in S'Range loop
            if I /= J and S(I) /= T(J) and S(J) /= T(I) then
               Tmp  := T(I);
               T(I) := T(J);
               T(J) := Tmp;
            end if;
         end loop;
      end loop;
      return T;
   end Best_Shuffle;

   Stop : Boolean := False;

begin -- main procedure
   while not Stop loop
      declare
         Original: String := Ada.Text_IO.Get_Line;
         Shuffle: String  := Best_Shuffle(Original);
         Score: Natural := 0;
      begin
         for I in Original'Range loop
            if Original(I) = Shuffle(I) then
            Score := Score + 1;
            end if;
         end loop;
         Ada.Text_Io.Put_Line(Original & ", " & Shuffle & ", (" &
                                Natural'Image(Score) & " )");
         if Original = "" then
            Stop := True;
         end if;
      end;
   end loop;
end Best_Shuffle;
