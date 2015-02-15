with Ada.Text_IO; use Ada.Text_IO;

procedure Pyramid_of_Numbers is

   B_X, B_Y, B_Z : Integer := 0; -- Unknown variables

   type Block_Value is record
      Known   : Integer := 0;
      X, Y, Z : Integer := 0;
   end record;
   X : constant Block_Value := (0, 1, 0, 0);
   Y : constant Block_Value := (0, 0, 1, 0);
   Z : constant Block_Value := (0, 0, 0, 1);
   procedure Add (L : in out Block_Value; R : Block_Value) is
   begin -- Symbolically adds one block to another
      L.Known := L.Known + R.Known;
      L.X := L.X + R.X - R.Z; -- Z is excluded as n(Y - X - Z) = 0
      L.Y := L.Y + R.Y + R.Z;
   end Add;
   procedure Add (L : in out Block_Value; R : Integer) is
   begin -- Symbolically adds a value to the block
      L.Known := L.Known + R;
   end Add;

   function Image (N : Block_Value) return String is
   begin -- The block value, when X,Y,Z are known
      return Integer'Image (N.Known + N.X * B_X + N.Y * B_Y + N.Z * B_Z);
   end Image;

   procedure Solve_2x2 (A11, A12, B1, A21, A22, B2 : Integer) is
   begin -- Don't care about things, supposing an integer solution exists
      if A22 = 0 then
         B_X := B2 / A21;
         B_Y := (B1 - A11*B_X) / A12;
      else
         B_X := (B1*A22 - B2*A12) / (A11*A22 - A21*A12);
         B_Y := (B1 - A11*B_X) / A12;
      end if;
      B_Z := B_Y - B_X;
   end Solve_2x2;

   B : array (1..5, 1..5) of Block_Value; -- The lower triangle contains blocks

begin
   -- The bottom blocks
   Add (B(5,1),X); Add (B(5,2),11); Add (B(5,3),Y); Add (B(5,4),4); Add (B(5,5),Z);

   -- Upward run
   for Row in reverse 1..4 loop
      for Column in 1..Row loop
         Add (B (Row, Column), B (Row + 1, Column));
         Add (B (Row, Column), B (Row + 1, Column + 1));
      end loop;
   end loop;

   -- Now have known blocks 40=(3,1), 151=(1,1) and Y=X+Z to determine X,Y,Z
   Solve_2x2
   (  B(1,1).X, B(1,1).Y, 151 - B(1,1).Known,
      B(3,1).X, B(3,1).Y,  40 - B(3,1).Known
   );

   -- Print the results
   for Row in 1..5 loop
      New_Line;
      for Column in 1..Row loop
         Put (Image (B(Row,Column)));
      end loop;
   end loop;
end Pyramid_of_Numbers;
