with Ada.Text_Io; use Ada.Text_Io;

procedure Sum_Series is
   function F(X : Long_Float) return Long_Float is
   begin
      return 1.0 / X**2;
   end F;
   package Lf_Io is new Ada.Text_Io.Float_Io(Long_Float);
   use Lf_Io;
   Sum : Long_Float := 0.0;
   subtype Param_Range is Integer range 1..1000;
begin
   for I in Param_Range loop
      Sum := Sum + F(Long_Float(I));
   end loop;
   Put("Sum of F(x) from" & Integer'Image(Param_Range'First) &
      " to" & Integer'Image(Param_Range'Last) & " is ");
   Put(Item => Sum, Aft => 10, Exp => 0);
   New_Line;
end Sum_Series;
