with Ada.Text_IO;

procedure Octal is
   package IIO is new Ada.Text_IO.Integer_IO(Integer);
begin
   for I in 0 .. Integer'Last loop
      IIO.Put(I, Base => 8);
      Ada.Text_IO.New_Line;
   end loop;
end Octal;
