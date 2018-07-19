with Ada.Text_IO;  use Ada.Text_IO;

procedure Queens is
   Board : array (1..8, 1..8) of Boolean := (others => (others => False));
   function Test (Row, Column : Integer) return Boolean is
   begin
      for J in 1..Column - 1 loop
         if (  Board (Row, J)
            or else
               (Row > J and then Board (Row - J, Column - J))
            or else
               (Row + J <= 8 and then Board (Row + J, Column - J))
            )  then
            return False;
         end if;
      end loop;
      return True;
   end Test;
   function Fill (Column : Integer) return Boolean is
   begin
      for Row in Board'Range (1) loop
         if Test (Row, Column) then
            Board (Row, Column) := True;
            if Column = 8 or else Fill (Column + 1) then
               return True;
            end if;
            Board (Row, Column) := False;
         end if;
      end loop;
      return False;
   end Fill;
begin
   if not Fill (1) then
      raise Program_Error;
   end if;
   for I in Board'Range (1) loop
      Put (Integer'Image (9 - I));
      for J in Board'Range (2) loop
         if Board (I, J) then
            Put ("|Q");
         elsif (I + J) mod 2 = 1 then
            Put ("|/");
         else
            Put ("| ");
         end if;
      end loop;
      Put_Line ("|");
   end loop;
   Put_Line ("   A B C D E F G H");
end Queens;
