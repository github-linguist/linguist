with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Real_Arrays; use Ada.Numerics.Real_Arrays;
with Ada.Numerics.Generic_Elementary_Functions;
procedure QR is

   procedure Show (mat : Real_Matrix) is
      package FIO is new Ada.Text_IO.Float_IO (Float);
   begin
      for row in mat'Range (1) loop
         for col in mat'Range (2) loop
            FIO.Put (mat (row, col), Exp => 0, Aft => 4, Fore => 5);
         end loop;
         New_Line;
      end loop;
   end Show;

   function GetCol (mat : Real_Matrix; n : Integer) return Real_Matrix is
      column : Real_Matrix (mat'Range (1), 1 .. 1);
   begin
      for row in mat'Range (1) loop
         column (row, 1) := mat (row, n);
      end loop;
      return column;
   end GetCol;

   function Mag (mat : Real_Matrix) return Float is
      sum : Real_Matrix := Transpose (mat) * mat;
      package Math is new Ada.Numerics.Generic_Elementary_Functions
         (Float);
   begin
      return Math.Sqrt (sum (1, 1));
   end Mag;

   function eVect (col : Real_Matrix; n : Integer) return Real_Matrix is
      vect : Real_Matrix (col'Range (1), 1 .. 1);
   begin
      for row in col'Range (1) loop
         if row /= n then vect (row, 1) := 0.0;
         else vect (row, 1) := 1.0; end if;
      end loop;
      return vect;
   end eVect;

   function Identity (n : Integer) return Real_Matrix is
      mat : Real_Matrix (1 .. n, 1 .. n) := (1 .. n => (others => 0.0));
   begin
      for i in Integer range 1 .. n loop mat (i, i) := 1.0; end loop;
      return mat;
   end Identity;

   function Chop (mat : Real_Matrix; n : Integer) return Real_Matrix is
      small : Real_Matrix (n .. mat'Length (1), n .. mat'Length (2));
   begin
      for row in small'Range (1) loop
         for col in small'Range (2) loop
            small (row, col) := mat (row, col);
         end loop;
      end loop;
      return small;
   end Chop;

   function H_n (inmat : Real_Matrix; n : Integer)
      return Real_Matrix is
      mat : Real_Matrix := Chop (inmat, n);
      col : Real_Matrix := GetCol (mat, n);
      colT : Real_Matrix (1 .. 1, mat'Range (1));
      H : Real_Matrix := Identity (mat'Length (1));
      Hall : Real_Matrix := Identity (inmat'Length (1));
   begin
      col := col - Mag (col) * eVect (col, n);
      col := col / Mag (col);
      colT := Transpose (col);
      H := H - 2.0 * (col * colT);
      for row in H'Range (1) loop
         for col in H'Range (2) loop
            Hall (n - 1 + row, n - 1 + col) := H (row, col);
         end loop;
      end loop;
      return Hall;
   end H_n;

   A : constant Real_Matrix (1 .. 3, 1 .. 3) := (
      (12.0, -51.0, 4.0),
      (6.0, 167.0, -68.0),
      (-4.0, 24.0, -41.0));
   Q1, Q2, Q3, Q, R: Real_Matrix (1 .. 3, 1 .. 3);
begin
   Q1 := H_n (A, 1);
   Q2 := H_n (Q1 * A, 2);
   Q3 := H_n (Q2 * Q1* A, 3);
   Q := Transpose (Q1) * Transpose (Q2) * TransPose(Q3);
   R := Q3 * Q2 * Q1 * A;
   Put_Line ("Q:"); Show (Q);
   Put_Line ("R:"); Show (R);
end QR;
