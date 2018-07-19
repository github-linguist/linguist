with Ada.Text_IO; with Ada.Numerics.Discrete_Random;

procedure Bias_Unbias is

   Modulus: constant Integer := 60; -- lcm of {3,4,5,6}
   type M is mod Modulus;
   package Rand is new Ada.Numerics.Discrete_Random(M);
   Gen: Rand.Generator;

   subtype Bit is Integer range 0 .. 1;

   function Biased_Bit(Bias_Base: Integer) return Bit is
   begin
      if (Integer(Rand.Random(Gen))* Bias_Base) / Modulus > 0 then
         return 0;
      else
         return 1;
      end if;
   end Biased_Bit;

   function Unbiased_Bit(Bias_Base: Integer) return Bit is
      A, B: Bit := 0;
   begin
      while A = B loop
         A := Biased_Bit(Bias_Base);
         B := Biased_Bit(Bias_Base);
      end loop;
      return A;
   end Unbiased_Bit;

   package FIO is new Ada.Text_IO.Float_IO(Float);

   Counter_B, Counter_U: Natural;
   Number_Of_Samples: constant Natural := 10_000;

begin
   Rand.Reset(Gen);
   Ada.Text_IO.Put_Line(" I  Biased% UnBiased%");
   for I in 3 .. 6 loop
      Counter_B := 0;
      Counter_U := 0;
      for J in 1 .. Number_Of_Samples loop
         Counter_B := Counter_B + Biased_Bit(I);
         Counter_U := Counter_U + Unbiased_Bit(I);
      end loop;
      Ada.Text_IO.Put(Integer'Image(I));
      FIO.Put(100.0 * Float(Counter_B) / Float(Number_Of_Samples), 5, 2, 0);
      FIO.Put(100.0 * Float(Counter_U) / Float(Number_Of_Samples), 5, 2, 0);
      Ada.Text_IO.New_Line;
   end loop;
end Bias_Unbias;
