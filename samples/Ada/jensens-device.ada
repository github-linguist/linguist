with Ada.Text_IO;  use Ada.Text_IO;

procedure Jensen_Device is
   function Sum
            (  I : not null access Float;
               Lo, Hi : Float;
               F : access function return Float
            )  return Float is
      Temp : Float := 0.0;
   begin
      I.all := Lo;
      while I.all <= Hi loop
         Temp := Temp + F.all;
         I.all := I.all + 1.0;
      end loop;
      return Temp;
   end Sum;

   I : aliased Float;
   function Inv_I return Float is
   begin
      return 1.0 / I;
   end Inv_I;
begin
   Put_Line (Float'Image (Sum (I'Access, 1.0, 100.0, Inv_I'Access)));
end Jensen_Device;
