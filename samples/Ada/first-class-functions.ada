with Ada.Float_Text_IO,
     Ada.Integer_Text_IO,
     Ada.Text_IO,
     Ada.Numerics.Elementary_Functions;

procedure First_Class_Functions is
   use Ada.Float_Text_IO,
       Ada.Integer_Text_IO,
       Ada.Text_IO,
       Ada.Numerics.Elementary_Functions;

   function Sqr (X : Float) return Float is
   begin
      return X ** 2;
   end Sqr;

   type A_Function is access function (X : Float) return Float;

   generic
      F, G : A_Function;
   function Compose (X : Float) return Float;

   function Compose (X : Float) return Float is
   begin
      return F (G (X));
   end Compose;

   Functions : array (Positive range <>) of A_Function := (Sin'Access,
                                                           Cos'Access,
                                                           Sqr'Access);
   Inverses  : array (Positive range <>) of A_Function := (Arcsin'Access,
                                                           Arccos'Access,
                                                           Sqrt'Access);
begin
   for I in Functions'Range loop
      declare
         function Identity is new Compose (Functions (I), Inverses (I));
         Test_Value : Float := 0.5;
         Result     : Float;
      begin
         Result := Identity (Test_Value);

         if Result = Test_Value then
            Put      ("Example ");
            Put      (I, Width => 0);
            Put_Line (" is perfect for the given test value.");
         else
            Put      ("Example ");
            Put      (I, Width => 0);
            Put      (" is off by");
            Put      (abs (Result - Test_Value));
            Put_Line (" for the given test value.");
         end if;
      end;
   end loop;
end First_Class_Functions;
