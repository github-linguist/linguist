with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

procedure Closest is
   package Math is new Ada.Numerics.Generic_Elementary_Functions (Float);

   Dimension : constant := 2;
   type Vector is array (1 .. Dimension) of Float;
   type Matrix is array (Positive range <>) of Vector;

   -- calculate the distance of two points
   function Distance (Left, Right : Vector) return Float is
      Result : Float := 0.0;
      Offset : Natural := 0;
   begin
      loop
         Result := Result + (Left(Left'First + Offset) - Right(Right'First + Offset))**2;
         Offset := Offset + 1;
         exit when Offset >= Left'Length;
      end loop;
      return Math.Sqrt (Result);
   end Distance;

   -- determine the two closest points inside a cloud of vectors
   function Get_Closest_Points (Cloud : Matrix) return Matrix is
      Result : Matrix (1..2);
      Min_Distance : Float;
   begin
      if Cloud'Length(1) < 2 then
         raise Constraint_Error;
      end if;
      Result := (Cloud (Cloud'First), Cloud (Cloud'First + 1));
      Min_Distance := Distance (Cloud (Cloud'First), Cloud (Cloud'First + 1));
      for I in Cloud'First (1) .. Cloud'Last(1) - 1 loop
         for J in I + 1 .. Cloud'Last(1) loop
            if Distance (Cloud (I), Cloud (J)) < Min_Distance then
               Min_Distance := Distance (Cloud (I), Cloud (J));
               Result := (Cloud (I), Cloud (J));
            end if;
         end loop;
      end loop;
      return Result;
   end Get_Closest_Points;

   Test_Cloud : constant Matrix (1 .. 10) := ( (5.0, 9.0),  (9.0, 3.0),
                                               (2.0, 0.0),  (8.0, 4.0),
                                               (7.0, 4.0),  (9.0, 10.0),
                                               (1.0, 9.0),  (8.0, 2.0),
                                               (0.0, 10.0), (9.0, 6.0));
   Closest_Points : Matrix := Get_Closest_Points (Test_Cloud);

   Second_Test : constant Matrix (1 .. 10) := ( (0.654682, 0.925557), (0.409382, 0.619391),
                                                (0.891663, 0.888594), (0.716629,   0.9962),
                                                (0.477721, 0.946355), (0.925092,  0.81822),
                                                (0.624291, 0.142924), (0.211332, 0.221507),
                                                (0.293786, 0.691701), (0.839186,  0.72826));
   Second_Points : Matrix := Get_Closest_Points (Second_Test);
begin
   Ada.Text_IO.Put_Line ("Closest Points:");
   Ada.Text_IO.Put_Line ("P1: " & Float'Image (Closest_Points (1) (1)) & " " & Float'Image (Closest_Points (1) (2)));
   Ada.Text_IO.Put_Line ("P2: " & Float'Image (Closest_Points (2) (1)) & " " & Float'Image (Closest_Points (2) (2)));
   Ada.Text_IO.Put_Line ("Distance: " & Float'Image (Distance (Closest_Points (1), Closest_Points (2))));
   Ada.Text_IO.Put_Line ("Closest Points 2:");
   Ada.Text_IO.Put_Line ("P1: " & Float'Image (Second_Points (1) (1)) & " " & Float'Image (Second_Points (1) (2)));
   Ada.Text_IO.Put_Line ("P2: " & Float'Image (Second_Points (2) (1)) & " " & Float'Image (Second_Points (2) (2)));
   Ada.Text_IO.Put_Line ("Distance: " & Float'Image (Distance (Second_Points (1), Second_Points (2))));
end Closest;
