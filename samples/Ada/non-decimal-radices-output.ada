with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Text_IO;          use Ada.Text_IO;

procedure Test_Integer_Text_IO is
begin
  for I in 1..33 loop
    Put (I, Width =>3, Base=> 10);
    Put (I, Width =>7, Base=> 16);
    Put (I, Width =>6, Base=>  8);
    New_Line;
  end loop;
end Test_Integer_Text_IO;
