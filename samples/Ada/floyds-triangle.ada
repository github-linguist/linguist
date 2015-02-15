with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Command_Line;

procedure Floyd_Triangle is

   Rows:    constant Positive := Integer'Value(Ada.Command_Line.Argument(1));
   Current:          Positive := 1;
   Width:            array(1 .. Rows) of Positive;

begin
   -- compute the width for the different columns
   for I in Width'Range loop
      Width(I) := Integer'Image(I + (Rows * (Rows-1))/2)'Length;
   end loop;

   -- output the triangle
   for Line in 1 .. Rows loop
      for Column in 1 .. Line loop
        Ada.Integer_Text_IO.Put(Current, Width => Width(Column));
        Current := Current + 1;
      end loop;
      Ada.Text_IO.New_Line;
   end loop;
end Floyd_Triangle;
