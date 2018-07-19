with Ada.Text_IO;
procedure main is
   Δ : Integer;
begin
   Δ := 41;
   Δ := Δ + 1;
   Ada.Text_IO.Put_Line (Δ'Img);
end main;
