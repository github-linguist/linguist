with Ada.Text_IO;
procedure Firstclass is
   generic
      n1, n2 : Float;
   function Multiplier (m : Float) return Float;
   function Multiplier (m : Float) return Float is begin
      return n1 * n2 * m;
   end Multiplier;

   num, inv : array (1 .. 3) of Float;
begin
   num := (2.0, 4.0, 6.0);
   inv := (1.0/2.0, 1.0/4.0, 1.0/6.0);
   for i in num'Range loop
      declare
         function new_function is new Multiplier (num (i), inv (i));
      begin
         Ada.Text_IO.Put_Line (new_function (0.5)'Img);
      end;
   end loop;
end Firstclass;
