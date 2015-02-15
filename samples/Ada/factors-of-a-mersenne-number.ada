with Ada.Text_IO;
--  reuse Is_Prime from [[Primality by Trial Division]]
with Is_Prime;

procedure Mersenne is
   function Is_Set (Number : Natural; Bit : Positive) return Boolean is
   begin
      return Number / 2 ** (Bit - 1) mod 2 = 1;
   end Is_Set;

   function Get_Max_Bit (Number : Natural) return Natural is
      Test : Natural := 0;
   begin
      while 2 ** Test <= Number loop
         Test := Test + 1;
      end loop;
      return Test;
   end Get_Max_Bit;

   function Modular_Power (Base, Exponent, Modulus : Positive) return Natural is
      Maximum_Bit : constant Natural := Get_Max_Bit (Exponent);
      Square      : Natural := 1;
   begin
      for Bit in reverse 1 .. Maximum_Bit loop
         Square := Square ** 2;
         if Is_Set (Exponent, Bit) then
            Square := Square * Base;
         end if;
         Square := Square mod Modulus;
      end loop;
      return Square;
   end Modular_Power;

   Not_A_Prime_Exponent : exception;

   function Get_Factor (Exponent : Positive) return Natural is
      Factor : Positive;
   begin
      if not Is_Prime (Exponent) then
         raise Not_A_Prime_Exponent;
      end if;
      for K in 1 .. 16384 / Exponent loop
         Factor := 2 * K * Exponent + 1;
         if Factor mod 8 = 1 or else Factor mod 8 = 7 then
            if Is_Prime (Factor) and then Modular_Power (2, Exponent, Factor) = 1 then
               return Factor;
            end if;
         end if;
      end loop;
      return 0;
   end Get_Factor;

   To_Test : constant Positive := 929;
   Factor  : Natural;
begin
   Ada.Text_IO.Put ("2 **" & Integer'Image (To_Test) & " - 1 ");
   begin
      Factor := Get_Factor (To_Test);
      if Factor = 0 then
         Ada.Text_IO.Put_Line ("is prime.");
      else
         Ada.Text_IO.Put_Line ("has factor" & Integer'Image (Factor));
      end if;
   exception
      when Not_A_Prime_Exponent =>
         Ada.Text_IO.Put_Line ("is not a Mersenne number");
   end;
end Mersenne;
