with Ada.Text_IO;

procedure Main is
   package Float_IO is new Ada.Text_IO.Float_IO (Float);
   function Van_Der_Corput (N : Natural; Base : Positive := 2) return Float is
      Value    : Natural  := N;
      Result   : Float    := 0.0;
      Exponent : Positive := 1;
   begin
      while Value > 0 loop
         Result   := Result +
                     Float (Value mod Base) / Float (Base ** Exponent);
         Value    := Value / Base;
         Exponent := Exponent + 1;
      end loop;
      return Result;
   end Van_Der_Corput;
begin
   for Base in 2 .. 5 loop
      Ada.Text_IO.Put ("Base" & Integer'Image (Base) & ":");
      for N in 1 .. 10 loop
         Ada.Text_IO.Put (' ');
         Float_IO.Put (Item => Van_Der_Corput (N, Base), Exp => 0);
      end loop;
      Ada.Text_IO.New_Line;
   end loop;
end Main;
