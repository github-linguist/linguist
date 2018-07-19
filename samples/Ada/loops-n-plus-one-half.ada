with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

procedure Loop_And_Half is
   I : Positive := 1;
begin
   loop
      Put(Item => I, Width => 1);
      exit when I = 10;
      Put(", ");
      I := I + 1;
   end loop;
   New_Line;
end Loop_And_Half;
