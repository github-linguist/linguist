with Ada.Text_IO;
procedure Map is
   type First_Range  is new Float range 0.0 .. 10.0;
   type Second_Range is new Float range -1.0 .. 0.0;
   function Translate (Value : First_Range) return Second_Range is
      B1 : Float := Float (Second_Range'First);
      B2 : Float := Float (Second_Range'Last);
      A1 : Float := Float (First_Range'First);
      A2 : Float := Float (First_Range'Last);
      Result : Float;
   begin
      Result := B1 + (Float (Value) - A1) * (B2 - B1) / (A2 - A1);
      return Second_Range (Result);
   end;
   function Translate (Value : Second_Range) return First_Range is
      B1 : Float := Float (First_Range'First);
      B2 : Float := Float (First_Range'Last);
      A1 : Float := Float (Second_Range'First);
      A2 : Float := Float (Second_Range'Last);
      Result : Float;
   begin
      Result := B1 + (Float (Value) - A1) * (B2 - B1) / (A2 - A1);
      return First_Range (Result);
   end;
   Test_Value : First_Range := First_Range'First;
begin
   loop
      Ada.Text_IO.Put_Line (First_Range'Image (Test_Value) & " maps to: "
                          & Second_Range'Image (Translate (Test_Value)));
      exit when Test_Value = First_Range'Last;
      Test_Value := Test_Value + 1.0;
   end loop;
end Map;
