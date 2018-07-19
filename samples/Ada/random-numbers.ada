with Ada.Numerics;                       use Ada.Numerics;
with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;

procedure Normal_Random is
   function Normal_Distribution
            (  Seed  : Generator;
               Mu    : Float := 1.0;
               Sigma : Float := 0.5
            )  return Float is
   begin
      return
         Mu + (Sigma * Sqrt (-2.0 * Log (Random (Seed), 10.0)) * Cos (2.0 * Pi * Random (Seed)));
   end Normal_Distribution;

   Seed         : Generator;
   Distribution : array (1..1_000) of Float;
begin
   Reset (Seed);
   for I in Distribution'Range loop
      Distribution (I) := Normal_Distribution (Seed);
   end loop;
end Normal_Random;
