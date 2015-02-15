with Ada.Numerics.Real_Arrays;  use Ada.Numerics.Real_Arrays;
with Ada.Text_IO;               use Ada.Text_IO;

procedure Matrix_Transpose is
   procedure Put (X : Real_Matrix) is
      type Fixed is delta 0.01 range -500.0..500.0;
   begin
      for I in X'Range (1) loop
         for J in X'Range (2) loop
            Put (Fixed'Image (Fixed (X (I, J))));
         end loop;
         New_Line;
      end loop;
   end Put;

   Matrix : constant Real_Matrix :=
            (  (0.0, 0.1, 0.2, 0.3),
               (0.4, 0.5, 0.6, 0.7),
               (0.8, 0.9, 1.0, 1.1)
            );
begin
   Put_Line ("Before Transposition:");
   Put (Matrix);
   New_Line;
   Put_Line ("After Transposition:");
   Put (Transpose (Matrix));
end Matrix_Transpose;
