with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Strings.Unbounded;

procedure Priority_Queues is
   use Ada.Containers;
   use Ada.Strings.Unbounded;
   type Queue_Element is record
      Priority : Natural;
      Content  : Unbounded_String;
   end record;
   function Get_Priority (Element : Queue_Element) return Natural is
   begin
      return Element.Priority;
   end Get_Priority;
   function Before (Left, Right : Natural) return Boolean is
   begin
      return Left > Right;
   end Before;
   package String_Queues is new Synchronized_Queue_Interfaces
     (Element_Type => Queue_Element);
   package String_Priority_Queues is new Unbounded_Priority_Queues
     (Queue_Interfaces => String_Queues,
      Queue_Priority => Natural);

   My_Queue : String_Priority_Queues.Queue;
begin
   My_Queue.Enqueue (New_Item => (Priority => 3, Content => To_Unbounded_String ("Clear drains")));
   My_Queue.Enqueue (New_Item => (Priority => 4, Content => To_Unbounded_String ("Feed cat")));
   My_Queue.Enqueue (New_Item => (Priority => 5, Content => To_Unbounded_String ("Make tea")));
   My_Queue.Enqueue (New_Item => (Priority => 1, Content => To_Unbounded_String ("Solve RC tasks")));
   My_Queue.Enqueue (New_Item => (Priority => 2, Content => To_Unbounded_String ("Tax return")));

   declare
      Element : Queue_Element;
   begin
      while My_Queue.Current_Use > 0 loop
         My_Queue.Dequeue (Element => Element);
         Ada.Text_IO.Put_Line (Natural'Image (Element.Priority) & " => " & To_String (Element.Content));
      end loop;
   end;
end Priority_Queues;
