with Ada.Text_IO;

procedure Binary_Output is

   package IIO is new Ada.Text_IO.Integer_IO(Integer);

   function To_Binary(N: Natural) return String is
      S: String(1 .. 1000); -- more than plenty!
      Left:  Positive := S'First;
      Right: Positive := S'Last;
   begin
      IIO.Put(To => S, Item => N, Base => 2); -- This is the conversion!
      -- Now S is a String with many spaces and some "2#...#" somewhere.
      -- We only need the "..." part without spaces or base markers.
      while S(Left) /= '#' loop
         Left := Left + 1;
      end loop;
      while S(Right) /= '#' loop
         Right := Right - 1;
      end loop;
      return S(Left+1 .. Right-1);
   end To_Binary;

begin
   Ada.Text_IO.Put_Line(To_Binary(5));    -- 101
   Ada.Text_IO.Put_Line(To_Binary(50));   -- 110010
   Ada.Text_IO.Put_Line(To_Binary(9000)); -- 10001100101000
end Binary_Output;
