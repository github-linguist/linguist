with Ada.Text_IO; use Ada.Text_IO;

procedure Fizzbuzz is
begin
   for I in 1..100 loop
      if I mod 15 = 0 then
         Put_Line("FizzBuzz");
      elsif I mod 5 = 0 then
         Put_Line("Buzz");
      elsif I mod 3 = 0 then
         Put_Line("Fizz");
      else
         Put_Line(Integer'Image(I));
      end if;
   end loop;
end Fizzbuzz;
