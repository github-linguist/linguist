with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_Io; use Ada.Text_Io;

procedure Traversal_Example is
   package Int_List is new Ada.Containers.Doubly_Linked_Lists(Integer);
   use Int_List;
   procedure Print(Position : Cursor) is
   begin
      Put_Line(Integer'Image(Element(Position)));
   end Print;
   The_List : List;
begin
   for I in 1..10 loop
      The_List.Append(I);
   end loop;
   -- Traverse the list, calling Print for each value
   The_List.Iterate(Print'access);
end traversal_example;
