with Ada.Command_Line;
with Ada.Text_IO;
with GNU_Multiple_Precision.Big_Integers;
with GNU_Multiple_Precision.Big_Rationals;
use GNU_Multiple_Precision;

procedure Pi_Digits is
   type Int is mod 2 ** 64;
   package Int_To_Big is new Big_Integers.Modular_Conversions (Int);

   -- constants
   Zero : constant Big_Integer := Int_To_Big.To_Big_Integer (0);
   One : constant Big_Integer := Int_To_Big.To_Big_Integer (1);
   Two : constant Big_Integer := Int_To_Big.To_Big_Integer (2);
   Three : constant Big_Integer := Int_To_Big.To_Big_Integer (3);
   Four : constant Big_Integer := Int_To_Big.To_Big_Integer (4);
   Ten : constant Big_Integer := Int_To_Big.To_Big_Integer (10);

   -- type LFT = (Integer, Integer, Integer, Integer
   type LFT is record
      Q, R, S, T : Big_Integer;
   end record;

   -- extr :: LFT -> Integer -> Rational
   function Extr (T : LFT; X : Big_Integer) return Big_Rational is
      use Big_Integers;
      Result : Big_Rational;
   begin
      -- extr (q,r,s,t) x = ((fromInteger q) * x + (fromInteger r)) /
      --                    ((fromInteger s) * x + (fromInteger t))
      Big_Rationals.Set_Numerator (Item         => Result,
                                   New_Value    => T.Q * X + T.R,
                                   Canonicalize => False);
      Big_Rationals.Set_Denominator (Item      => Result,
                                     New_Value => T.S * X + T.T);
      return Result;
   end Extr;

   -- unit :: LFT
   function Unit return LFT is
   begin
      -- unit = (1,0,0,1)
      return LFT'(Q => One, R => Zero, S => Zero, T => One);
   end Unit;

   -- comp :: LFT -> LFT -> LFT
   function Comp (T1, T2 : LFT) return LFT is
      use Big_Integers;
   begin
      -- comp (q,r,s,t) (u,v,w,x) = (q*u+r*w,q*v+r*x,s*u+t*w,s*v+t*x)
      return LFT'(Q => T1.Q * T2.Q + T1.R * T2.S,
                  R => T1.Q * T2.R + T1.R * T2.T,
                  S => T1.S * T2.Q + T1.T * T2.S,
                  T => T1.S * T2.R + T1.T * T2.T);
   end Comp;

   -- lfts = [(k, 4*k+2, 0, 2*k+1) | k<-[1..]
   K : Big_Integer := Zero;
   function LFTS return LFT is
      use Big_Integers;
   begin
      K := K + One;
      return LFT'(Q => K,
                  R => Four * K + Two,
                  S => Zero,
                  T => Two * K + One);
   end LFTS;

   -- next z = floor (extr z 3)
   function Next (T : LFT) return Big_Integer is
   begin
      return Big_Rationals.To_Big_Integer (Extr (T, Three));
   end Next;

   -- safe z n = (n == floor (extr z 4)
   function Safe (T : LFT; N : Big_Integer) return Boolean is
   begin
      return N = Big_Rationals.To_Big_Integer (Extr (T, Four));
   end Safe;

   -- prod z n = comp (10, -10*n, 0, 1)
   function Prod (T : LFT; N : Big_Integer) return LFT is
      use Big_Integers;
   begin
      return Comp (LFT'(Q => Ten, R => -Ten * N, S => Zero, T => One), T);
   end Prod;

   procedure Print_Pi (Digit_Count : Positive) is
      Z : LFT := Unit;
      Y : Big_Integer;
      Count : Natural := 0;
   begin
      loop
         Y := Next (Z);
         if Safe (Z, Y) then
            Count := Count + 1;
            Ada.Text_IO.Put (Big_Integers.Image (Y));
            exit when Count >= Digit_Count;
            Z := Prod (Z, Y);
         else
            Z := Comp (Z, LFTS);
         end if;
      end loop;
   end Print_Pi;

   N : Positive := 250;
begin
   if Ada.Command_Line.Argument_Count = 1 then
      N := Positive'Value (Ada.Command_Line.Argument (1));
   end if;
   Print_Pi (N);
end Pi_Digits;
