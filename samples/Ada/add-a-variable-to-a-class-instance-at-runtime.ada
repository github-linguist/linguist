with Ada.Text_IO;  use Ada.Text_IO;

procedure Dynamic is
   package Abstract_Class is
      type Class is limited interface;
      function Boo (X : Class) return String is abstract;
   end Abstract_Class;
   use Abstract_Class;

   package Base_Class is
      type Base is new Class with null record;
      overriding function Boo (X : Base) return String;
   end Base_Class;

   package body Base_Class is
      function Boo (X : Base) return String is
      begin
         return "I am Class";
      end Boo;
   end Base_Class;
   use Base_Class;

   E : aliased Base;  -- An instance of Base

begin
   -- Gone run-time
   declare
      type Monkey_Patch (Root : access Base) is new Class with record
         Foo : Integer := 1;
      end record;
      overriding function Boo (X : Monkey_Patch) return String;
      function Boo (X : Monkey_Patch) return String is
      begin -- Delegation to the base
         return X.Root.Boo;
      end Boo;
      EE : Monkey_Patch (E'Access); -- Extend E
   begin
      Put_Line (EE.Boo & " with" & Integer'Image (EE.Foo));
   end;
end Dynamic;
