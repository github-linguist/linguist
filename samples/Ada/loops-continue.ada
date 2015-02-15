with Ada.Text_Io; use Ada.Text_Io;

procedure Loop_Continue is
begin
   for I in 1..10 loop
      Put(Integer'Image(I));
      if I mod 5 = 0 then
         New_Line;
      else
         Put(",");
      end if;
   end loop;
end Loop_Continue;
