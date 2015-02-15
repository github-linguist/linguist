with Ada.Text_Io; use Ada.Text_Io;

procedure Counting_Sort is
   type Data is array (Integer range <>) of Natural;
   procedure Sort(Item : out Data) is
   begin
      for I in Item'range loop
         Item(I) := I;
      end loop;
   end Sort;
   Stuff : Data(1..140);
begin
   Sort(Stuff);
   for I in Stuff'range loop
      Put(Natural'Image(Stuff(I)));
   end loop;
   New_Line;
end Counting_Sort;
