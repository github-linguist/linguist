with Ada.Text_IO, Ada.Numerics.Generic_Elementary_Functions;

procedure Arith_Geom_Mean is

   type Num is digits 18; -- the largest value gnat/gcc allows
   package N_IO is new Ada.Text_IO.Float_IO(Num);
   package Math is new Ada.Numerics.Generic_Elementary_Functions(Num);

   function AGM(A, G: Num) return Num is
      Old_G: Num;
      New_G: Num := G;
      New_A: Num := A;
   begin
      loop
         Old_G := New_G;
         New_G := Math.Sqrt(New_A*New_G);
         New_A := (Old_G + New_A) * 0.5;
         exit when (New_A - New_G) <= Num'Epsilon;
         -- Num'Epsilon denotes the relative error when performing arithmetic over Num
      end loop;
      return New_G;
   end AGM;

begin
   N_IO.Put(AGM(1.0, 1.0/Math.Sqrt(2.0)), Fore => 1, Aft => 17, Exp => 0);
end Arith_Geom_Mean;
