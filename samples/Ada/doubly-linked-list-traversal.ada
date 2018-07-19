with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

procedure Traversing is
   package Char_Lists is new Ada.Containers.Doubly_Linked_Lists (Character);

   procedure Print (Position : in Char_Lists.Cursor) is
   begin
      Ada.Text_IO.Put (Char_Lists.Element (Position));
   end Print;

   My_List : Char_Lists.List;
begin
   My_List.Append ('R');
   My_List.Append ('o');
   My_List.Append ('s');
   My_List.Append ('e');
   My_List.Append ('t');
   My_List.Append ('t');
   My_List.Append ('a');

   My_List.Iterate (Print'Access);
   Ada.Text_IO.New_Line;

   My_List.Reverse_Iterate (Print'Access);
   Ada.Text_IO.New_Line;
end Traversing;
