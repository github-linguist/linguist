-- Divide By Zero Detection

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

procedure Divide_By_Zero is
   Fnum : Float := 1.0;
   Fdenom : Float := 0.0;
   Fresult : Float;
   Inum : Integer := 1;
   Idenom : Integer := 0;
   Iresult : Integer;
begin
   begin
      Put("Integer divide by zero: ");
      Iresult := Inum / Idenom;
      Put(Item => Iresult);
   exception
      when Constraint_Error =>
         Put("Division by zero detected.");
   end;
   New_Line;
   Put("Floating point divide by zero: ");
   Fresult := Fnum / Fdenom;
   if Fresult > Float'Last then
      Put("Division by zero detected (infinite value).");
   else
      Put(Item => Fresult, Aft => 9, Exp => 0);
   end if;
   New_Line;
end Divide_By_Zero;
