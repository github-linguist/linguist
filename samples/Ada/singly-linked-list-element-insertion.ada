with Ada.Unchecked_Deallocation;
-- Define the link type
procedure Singly_Linked is

   type Link;
   type Link_Access is access Link;
   type Link is record
      Data : Integer;
      Next : Link_Access := null;
   end record;
   -- Instantiate the generic deallocator for the link type
   procedure Free is new Ada.Unchecked_Deallocation(Link, Link_Access);

   -- Define the procedure
   procedure Insert_Append(Anchor : Link_Access; Newbie : Link_Access) is
   begin
      if Anchor /= null and Newbie /= null then
         Newbie.Next := Anchor.Next;
         Anchor.Next := Newbie;
      end if;
   end Insert_Append;

   -- Create the link elements
   A : Link_Access := new Link'(1, null);
   B : Link_Access := new Link'(2, null);
   C : Link_Access := new Link'(3, null);
-- Execute the program
begin
   Insert_Append(A, B);
   Insert_Append(A, C);
   Free(A);
   Free(B);
   Free(C);
end Singly_Linked;
