with Ada.Text_IO;  use Ada.Text_IO;

procedure Power_Set is
   type Universe is (A,B,C,D,E);

   -- The type Set are subsets of Universe
   type Set is array (Universe) of Boolean;
   Empty : constant Set := (others => False);
   function Cardinality (X : Set) return Natural is
      N : Natural := 0;
   begin
      for I in X'Range loop
         if X (I) then
            N := N + 1;
         end if;
      end loop;
      return N;
   end Cardinality;
   function Element (X : Set; Position : Positive) return Universe is
      N : Natural := 0;
   begin
      for I in X'Range loop
         if X (I) then
            N := N + 1;
            if N = Position then
               return I;
            end if;
         end if;
      end loop;
      raise Constraint_Error;
   end Element;
   procedure Put (X : Set) is
      Empty : Boolean := True;
   begin
      for I in X'Range loop
         if X (I) then
            if Empty then
               Empty := False;
               Put (Universe'Image (I));
            else
               Put ("," & Universe'Image (I));
            end if;
         end if;
      end loop;
      if Empty then
         Put ("empty");
      end if;
   end Put;

   -- Set_Of_Set are sets of subsets of Universe
   type Set_Of_Sets is array (Positive range <>) of Set;

   function Power (X : Set) return Set_Of_Sets is
      Length : constant Natural := Cardinality (X);
      Index  : array (1..Length) of Integer := (others => 0);
      Result : Set_Of_Sets (1..2**Length)   := (others => Empty);
   begin
      for N in Result'Range loop
         for I in 1..Length loop -- Index determines the sample N
            exit when Index (I) = 0;
            Result (N) (Element (X, Index (I))) := True;
         end loop;
  Next : for I in 1..Length loop -- Computing the index of the following sample
            if Index (I) < Length then
               Index (I) := Index (I) + 1;
               if I = 1 or else Index (I - 1) > Index (I) then
                  for J in reverse 2..I loop
                     Index (J - 1) := Index (J) + 1;
                  end loop;
                  exit Next;
               end if;
            end if;
         end loop Next;
      end loop;
      return Result;
   end Power;

   P : Set_Of_Sets := Power ((A|C|E => True, others => False));
begin
   for I in P'Range loop
      New_Line;
      Put (P (I));
   end loop;
end Power_Set;
