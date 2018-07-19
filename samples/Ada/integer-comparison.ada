with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_Io;

procedure Compare_Ints is
   A, B : Integer;
begin
   Get(Item => A);
   Get(Item => B);

   -- Test for equality
   if A = B then
      Put_Line("A equals B");
   end if;

   -- Test For Less Than
   if A < B then
      Put_Line("A is less than B");
   end if;

   -- Test For Greater Than
   if A > B then
      Put_Line("A is greater than B");
   end if;
end Compare_Ints;
