with Ada.Text_IO;

procedure Pythagorean_Triples is

   type Large_Natural is range 0 .. 2**63-1;
     -- this is the maximum for gnat

   procedure New_Triangle(A, B, C: Large_Natural;
                          Max_Perimeter: Large_Natural;
                          Total_Cnt, Primitive_Cnt: in out Large_Natural) is
      Perimeter: constant Large_Natural := A + B + C;
   begin
      if Perimeter <= Max_Perimeter then
         Primitive_Cnt := Primitive_Cnt + 1;
         Total_Cnt     := Total_Cnt + Max_Perimeter / Perimeter;
         New_Triangle(A-2*B+2*C,     2*A-B+2*C,    2*A-2*B+3*C,   Max_Perimeter, Total_Cnt, Primitive_Cnt);
         New_Triangle(A+2*B+2*C,     2*A+B+2*C,    2*A+2*B+3*C,   Max_Perimeter, Total_Cnt, Primitive_Cnt);
         New_Triangle(2*B+2*C-A,     B+2*C-2*A,    2*B+3*C-2*A,   Max_Perimeter, Total_Cnt, Primitive_Cnt);
      end if;
   end New_Triangle;

   T_Cnt, P_Cnt: Large_Natural;

begin
   for I in 1 .. 9 loop
      T_Cnt := 0;
      P_Cnt := 0;
      New_Triangle(3,4,5, 10**I, Total_Cnt => T_Cnt, Primitive_Cnt => P_Cnt);
      Ada.Text_IO.Put_Line("Up to 10 **" & Integer'Image(I) & " :" &
                             Large_Natural'Image(T_Cnt) & " Triples," &
                             Large_Natural'Image(P_Cnt) & " Primitives");
   end loop;
end Pythagorean_Triples;
