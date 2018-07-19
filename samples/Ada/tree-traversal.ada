with Ada.Text_Io; use Ada.Text_Io;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Doubly_Linked_Lists;

procedure Tree_Traversal is
   type Node;
   type Node_Access is access Node;
   type Node is record
      Left : Node_Access := null;
      Right : Node_Access := null;
      Data : Integer;
   end record;
   procedure Destroy_Tree(N : in out Node_Access) is
      procedure free is new Ada.Unchecked_Deallocation(Node, Node_Access);
   begin
      if N.Left /= null then
         Destroy_Tree(N.Left);
      end if;
      if N.Right /= null then
         Destroy_Tree(N.Right);
      end if;
      Free(N);
   end Destroy_Tree;
   function Tree(Value : Integer; Left : Node_Access; Right : Node_Access) return Node_Access is
      Temp : Node_Access := new Node;
   begin
      Temp.Data := Value;
      Temp.Left := Left;
      Temp.Right := Right;
      return Temp;
   end Tree;
   procedure Preorder(N : Node_Access) is
   begin
      Put(Integer'Image(N.Data));
      if N.Left /= null then
         Preorder(N.Left);
      end if;
      if N.Right /= null then
         Preorder(N.Right);
      end if;
   end Preorder;
   procedure Inorder(N : Node_Access) is
   begin
      if N.Left /= null then
         Inorder(N.Left);
      end if;
      Put(Integer'Image(N.Data));
      if N.Right /= null then
         Inorder(N.Right);
      end if;
   end Inorder;
   procedure Postorder(N : Node_Access) is
   begin
      if N.Left /= null then
         Postorder(N.Left);
      end if;
      if N.Right /= null then
         Postorder(N.Right);
      end if;
      Put(Integer'Image(N.Data));
   end Postorder;
   procedure Levelorder(N : Node_Access) is
      package Queues is new Ada.Containers.Doubly_Linked_Lists(Node_Access);
      use Queues;
      Node_Queue : List;
      Next : Node_Access;
   begin
      Node_Queue.Append(N);
      while not Is_Empty(Node_Queue) loop
         Next := First_Element(Node_Queue);
         Delete_First(Node_Queue);
         Put(Integer'Image(Next.Data));
         if Next.Left /= null then
            Node_Queue.Append(Next.Left);
         end if;
         if Next.Right /= null then
            Node_Queue.Append(Next.Right);
         end if;
      end loop;
   end Levelorder;
   N : Node_Access;
begin
   N := Tree(1,
      Tree(2,
         Tree(4,
            Tree(7, null, null),
            null),
         Tree(5, null, null)),
      Tree(3,
         Tree(6,
            Tree(8, null, null),
            Tree(9, null, null)),
         null));

   Put("preorder:    ");
   Preorder(N);
   New_Line;
   Put("inorder:     ");
   Inorder(N);
   New_Line;
   Put("postorder:   ");
   Postorder(N);
   New_Line;
   Put("level order: ");
   Levelorder(N);
   New_Line;
   Destroy_Tree(N);
end Tree_traversal;
