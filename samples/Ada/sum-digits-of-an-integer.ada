with Ada.Integer_Text_IO;

procedure Sum_Digits is
   -- sums the digits of an integer (in whatever base)
   -- outputs the sum (in base 10)

   function Sum_Of_Digits(N: Natural; Base: Natural := 10) return Natural is
      Sum: Natural := 0;
      Val: Natural := N;
   begin
      while Val > 0 loop
         Sum := Sum + (Val mod Base);
         Val := Val / Base;
      end loop;
      return Sum;
   end Sum_Of_Digits;

   use Ada.Integer_Text_IO;

begin -- main procedure Sum_Digits
   Put(Sum_OF_Digits(1));            --   1
   Put(Sum_OF_Digits(12345));        --  15
   Put(Sum_OF_Digits(123045));       --  15
   Put(Sum_OF_Digits(123045,  50));  -- 104
   Put(Sum_OF_Digits(16#fe#,  10));  --  11
   Put(Sum_OF_Digits(16#fe#,  16));  --  29
   Put(Sum_OF_Digits(16#f0e#, 16));  --  29
end Sum_Digits;
