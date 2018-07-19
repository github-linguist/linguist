with Ada.Text_IO;  use Ada.Text_IO;

procedure Delegation is
   package Things is
      -- We need a common root for our stuff
      type Object is tagged null record;
      type Object_Ptr is access all Object'Class;

      -- Objects that have operation thing
      type Substantial is new Object with null record;
      function Thing (X : Substantial) return String;

      -- Delegator objects
      type Delegator is new Object with record
         Delegate : Object_Ptr;
      end record;
      function Operation (X : Delegator) return String;

      No_Thing  : aliased Object;      -- Does not have thing
      Has_Thing : aliased Substantial; -- Has one
   end Things;

   package body Things is
      function Thing (X : Substantial) return String is
      begin
         return "delegate implementation";
      end Thing;

      function Operation (X : Delegator) return String is
      begin
         if X.Delegate /= null and then X.Delegate.all in Substantial'Class then
            return Thing (Substantial'Class (X.Delegate.all));
         else
            return "default implementation";
         end if;
      end Operation;
   end Things;

   use Things;

   A : Delegator; -- Without a delegate
begin
   Put_Line (A.Operation);
   A.Delegate := No_Thing'Access; -- Set no thing
   Put_Line (A.Operation);
   A.Delegate := Has_Thing'Access; -- Set a thing
   Put_Line (A.Operation);
end Delegation;
