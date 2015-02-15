with Ada.Text_IO;

procedure Main is
   function Levenshtein_Distance (S, T : String) return Natural is
      D : array (0 .. S'Length, 0 .. T'Length) of Natural;
   begin
      for I in D'Range (1) loop
         D (I, 0) := I;
      end loop;
      for I in D'Range (2) loop
         D (0, I) := I;
      end loop;
      for J in T'Range loop
         for I in S'Range loop
            if S (I) = T (J) then
               D (I, J) := D (I - 1, J - 1);
            else
               D (I, J) :=
                  Natural'Min
                    (Natural'Min (D (I - 1, J) + 1, D (I, J - 1) + 1),
                     D (I - 1, J - 1) + 1);
            end if;
         end loop;
      end loop;
      return D (S'Length, T'Length);
   end Levenshtein_Distance;
begin
   Ada.Text_IO.Put_Line
     ("kitten -> sitting:" &
      Integer'Image (Levenshtein_Distance ("kitten", "sitting")));
   Ada.Text_IO.Put_Line
     ("rosettacode -> raisethysword:" &
      Integer'Image (Levenshtein_Distance ("rosettacode", "raisethysword")));
end Main;
