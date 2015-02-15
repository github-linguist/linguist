with Ada.Text_IO;

procedure Vector is
   type Float_Vector is array (Positive range <>) of Float;
   package Float_IO is new Ada.Text_IO.Float_IO (Float);

   procedure Vector_Put (X : Float_Vector) is
   begin
      Ada.Text_IO.Put ("(");
      for I in X'Range loop
         Float_IO.Put (X (I), Aft => 1, Exp => 0);
         if I /= X'Last then
            Ada.Text_IO.Put (", ");
         end if;
      end loop;
      Ada.Text_IO.Put (")");
   end Vector_Put;

   -- cross product
   function "*" (Left, Right : Float_Vector) return Float_Vector is
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error with "vectors of different size in dot product";
      end if;
      if Left'Length /= 3 then
         raise Constraint_Error with "dot product only implemented for R**3";
      end if;
      return Float_Vector'(Left (Left'First + 1) * Right (Right'First + 2) -
                             Left (Left'First + 2) * Right (Right'First + 1),
                           Left (Left'First + 2) * Right (Right'First) -
                             Left (Left'First) * Right (Right'First + 2),
                           Left (Left'First) * Right (Right'First + 1) -
                             Left (Left'First + 1) * Right (Right'First));
   end "*";

   -- scalar product
   function "*" (Left, Right : Float_Vector) return Float is
      Result : Float := 0.0;
      I, J : Positive;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error with "vectors of different size in scalar product";
      end if;
      I := Left'First; J := Right'First;
      while I <= Left'Last and then J <= Right'Last loop
         Result := Result + Left (I) * Right (J);
         I := I + 1; J := J + 1;
      end loop;
      return Result;
   end "*";

   -- stretching
   function "*" (Left : Float_Vector; Right : Float) return Float_Vector is
      Result : Float_Vector (Left'Range);
   begin
      for I in Left'Range loop
         Result (I) := Left (I) * Right;
      end loop;
      return Result;
   end "*";

   A : constant Float_Vector := (3.0, 4.0, 5.0);
   B : constant Float_Vector := (4.0, 3.0, 5.0);
   C : constant Float_Vector := (-5.0, -12.0, -13.0);
begin
   Ada.Text_IO.Put ("A: "); Vector_Put (A); Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("B: "); Vector_Put (B); Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("C: "); Vector_Put (C); Ada.Text_IO.New_Line;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("A dot B = "); Float_IO.Put (A * B, Aft => 1, Exp => 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("A x B = "); Vector_Put (A * B);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("A dot (B x C) = "); Float_IO.Put (A * (B * C), Aft => 1, Exp => 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("A x (B x C) = "); Vector_Put (A * Float_Vector'(B * C));
   Ada.Text_IO.New_Line;
end Vector;
