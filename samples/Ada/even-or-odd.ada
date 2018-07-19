-- Ada has bitwise operators in package Interfaces,
-- but they work with Interfaces.Unsigned_*** types only.
-- Use rem or mod for Integer types, and let the compiler
-- optimize it.
declare
   N : Integer := 5;
begin
   if N rem 2 = 0 then
      Put_Line ("Even number");
   elseif N rem 2 /= 0 then
      Put_Line ("Odd number");
   else
      Put_Line ("Something went really wrong!");
   end if;
end;
