with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Polymorphic_Copy is
   package Base is
      type T is tagged null record;
      function Name (X : T) return String;
   end Base;
   use Base;

   package body Base is
      function Name (X : T) return String is
      begin
         return "T";
      end Name;
   end Base;

      -- The procedure knows nothing about S
   procedure Copier (X : T'Class) is
      Duplicate : T'Class := X;  -- A copy of X
   begin
      Put_Line ("Copied " & Duplicate.Name); -- Check the copy
   end Copier;

   package Derived is
      type S is new T with null record;
      overriding function Name (X : S) return String;
   end Derived;
   use Derived;

   package body Derived is
      function Name (X : S) return String is
      begin
         return "S";
      end Name;
   end Derived;

   Object_1 : T;
   Object_2 : S;
begin
   Copier (Object_1);
   Copier (Object_2);
end Test_Polymorphic_Copy;
