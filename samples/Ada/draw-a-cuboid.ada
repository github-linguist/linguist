with Ada.Text_IO;

procedure Main is
   type Char_Matrix is
     array (Positive range <>, Positive range <>) of Character;

   function Create_Cuboid
     (Width, Height, Depth : Positive)
      return                 Char_Matrix
   is
      Result : Char_Matrix (1 .. Height + Depth + 3,
         1 .. 2 * Width + Depth + 3) := (others => (others => ' '));
   begin
      -- points
      Result (1, 1)                                      := '+';
      Result (Height + 2, 1)                             := '+';
      Result (1, 2 * Width + 2)                          := '+';
      Result (Height + 2, 2 * Width + 2)                 := '+';
      Result (Height + Depth + 3, Depth + 2)             := '+';
      Result (Depth + 2, 2 * Width + Depth + 3)          := '+';
      Result (Height + Depth + 3, 2 * Width + Depth + 3) := '+';
      -- width lines
      for I in 1 .. 2 * Width loop
         Result (1, I + 1)                          := '-';
         Result (Height + 2, I + 1)                 := '-';
         Result (Height + Depth + 3, Depth + I + 2) := '-';
      end loop;
      -- height lines
      for I in 1 .. Height loop
         Result (I + 1, 1)                             := '|';
         Result (I + 1, 2 * Width + 2)                 := '|';
         Result (Depth + I + 2, 2 * Width + Depth + 3) := '|';
      end loop;
      -- depth lines
      for I in 1 .. Depth loop
         Result (Height + 2 + I, 1 + I)             := '/';
         Result (1 + I, 2 * Width + 2 + I)          := '/';
         Result (Height + 2 + I, 2 * Width + 2 + I) := '/';
      end loop;
      return Result;
   end Create_Cuboid;

   procedure Print_Cuboid (Width, Height, Depth : Positive) is
      Cuboid : Char_Matrix := Create_Cuboid (Width, Height, Depth);
   begin
      for Row in reverse Cuboid'Range (1) loop
         for Col in Cuboid'Range (2) loop
            Ada.Text_IO.Put (Cuboid (Row, Col));
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Print_Cuboid;
begin
   Print_Cuboid (2, 3, 4);
end Main;
