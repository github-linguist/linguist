declare
   I : Integer := 1024;
begin
   while I > 0 loop
      Put_Line(Integer'Image(I));
      I := I / 2;
   end loop;
end;
