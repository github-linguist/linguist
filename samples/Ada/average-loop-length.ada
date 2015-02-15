with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Discrete_Random;
procedure Avglen is
   package IIO is new Ada.Text_IO.Integer_IO (Positive); use IIO;
   package LFIO is new Ada.Text_IO.Float_IO (Long_Float); use LFIO;
   subtype FactN is Natural range 0..20;
   TESTS : constant Natural := 1_000_000;

   function Factorial (N : FactN) return Long_Float is
      Result : Long_Float := 1.0;
   begin
      for I in 2..N loop Result := Result * Long_Float(I); end loop;
      return Result;
   end Factorial;

   function Analytical (N : FactN) return Long_Float is
      Sum : Long_Float := 0.0;
   begin
      for I in 1..N loop
         Sum := Sum + Factorial(N) / Factorial(N - I) / Long_Float(N)**I;
      end loop;
      return Sum;
   end Analytical;

   function Experimental (N : FactN) return Long_Float is
      subtype RandInt is Natural range 1..N;
      package Random is new Ada.Numerics.Discrete_Random(RandInt);
      seed : Random.Generator;
      Num : RandInt;
      count : Natural := 0;
      bits : array(RandInt'Range) of Boolean;
   begin
      Random.Reset(seed);
      for run in 1..TESTS loop
         bits := (others  => false);
         for I in RandInt'Range loop
            Num := Random.Random(seed); exit when bits(Num);
            bits(Num) := True; count := count + 1;
         end loop;
      end loop;
      return Long_Float(count)/Long_Float(TESTS);
   end Experimental;

   A, E, err : Long_Float;
begin
   Put_Line(" N  avg    calc   %diff");
   for I in 1..20 loop
      A := Analytical(I);  E := Experimental(I); err := abs(E-A)/A*100.0;
      Put(I, Width=>2); Put(E ,Aft=>4, exp=>0); Put(A, Aft=>4, exp=>0);
      Put(err, Fore=>3, Aft=>3, exp=>0); New_line;
   end loop;
end Avglen;
