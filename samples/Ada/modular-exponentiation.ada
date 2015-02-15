with Ada.Text_IO, Ada.Command_Line, Crypto.Types.Big_Numbers;

procedure Mod_Exp is

   A: String :=
     "2988348162058574136915891421498819466320163312926952423791023078876139";
   B: String :=
     "2351399303373464486466122544523690094744975233415544072992656881240319";

   D: constant Positive := Positive'Max(Positive'Max(A'Length, B'Length), 40);
     -- the number of decimals to store A, B, and result
   Bits: constant Positive := (34*D)/10;
     -- (slightly more than) the number of bits to store A, B, and result
   package LN is new Crypto.Types.Big_Numbers (Bits + (32 - Bits mod 32));
     -- the actual number of bits has to be a multiple of 32
   use type LN.Big_Unsigned;

   function "+"(S: String) return LN.Big_Unsigned
     renames LN.Utils.To_Big_Unsigned;

   M: LN.Big_Unsigned := (+"10") ** (+"40");

begin
   Ada.Text_IO.Put("A**B (mod 10**40) = ");
   Ada.Text_IO.Put_Line(LN.Utils.To_String(LN.Mod_Utils.Pow((+A), (+B), M)));
end Mod_Exp;
