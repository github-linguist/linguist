with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Text_Io; use Ada.Text_Io;

procedure Pascals_Triangle is
   type Row is array(Positive range <>) of Integer;
   type Row_Access is access Row;
   type Triangle is array(Positive range <>) of Row_Access;
   function General_Triangle(Depth : Positive) return Triangle is
      Result : Triangle(1..Depth);
   begin
      for I in Result'range loop
         Result(I) := new Row(1..I);
         for J in 1..I loop
            if J = Result(I)'First or else J = Result(I)'Last then
               Result(I)(J) := 1;
            else
               Result(I)(J) := Result(I - 1)(J - 1) + Result(I - 1)(J);
            end if;
         end loop;
      end loop;
      return Result;
   end General_Triangle;
   procedure Print(Item : Triangle) is
   begin
      for I in Item'range loop
         for J in 1..I loop
            Put(Item => Item(I)(J), Width => 3);
         end loop;
         New_Line;
      end loop;
   end Print;
begin
   Print(General_Triangle(7));
end Pascals_Triangle;
