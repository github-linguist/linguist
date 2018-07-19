with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

procedure VLQ is

   package Nat_IO is new Ada.Text_IO.Integer_IO (Natural);

   type Byte is mod 2**8;

   package Byte_IO is new Ada.Text_IO.Modular_IO (Byte);

   type Int7 is mod 2**7;

   package Int7_IO is new Ada.Text_IO.Modular_IO (Int7);

   type VLQ_Octet is record
      Value : Int7 := 0;
      Next  : Boolean := True;
   end record;
   pragma Pack (VLQ_Octet);
   for VLQ_Octet'Size use 8;

   function VLQ_To_Byte is new Ada.Unchecked_Conversion (VLQ_Octet, Byte);
   function Byte_To_VLQ is new Ada.Unchecked_Conversion (Byte, VLQ_Octet);

   package VLQ_Vectors is new Ada.Containers.Vectors (Natural, VLQ_Octet);

   procedure Hex_Print (Position : in VLQ_Vectors.Cursor) is
      Value : Byte := VLQ_To_Byte (VLQ_Vectors.Element (Position));
   begin
      Ada.Text_IO.Put (':');
      Byte_IO.Put (Item => Value, Width => 6, Base => 16);
   end Hex_Print;

   procedure Print (X : VLQ_Vectors.Vector) is
   begin
      X.Iterate (Hex_Print'Access);
      Ada.Text_IO.New_Line;
   end Print;

   function To_VLQ (From : Natural) return VLQ_Vectors.Vector is
      Result : VLQ_Vectors.Vector;
      Current : Natural := From;
      Element : VLQ_Octet;
   begin
      loop
         Element.Value := Int7 (Current mod 2**7);
         Result.Prepend (Element);
         Current := Current / 2**7;
         exit when Current = 0;
      end loop;
      Element := Result.Last_Element;
      Element.Next := False;
      VLQ_Vectors.Replace_Element (Result, Result.Last, Element);
      return Result;
   end To_VLQ;

   function To_Int (From : VLQ_Vectors.Vector) return Natural is
      use type VLQ_Vectors.Cursor;
      Result : Natural := 0;
      Iterator : VLQ_Vectors.Cursor := From.First;
   begin
      while Iterator /= VLQ_Vectors.No_Element loop
         Result := Result * 2**7;
         Result := Result + Natural(VLQ_Vectors.Element (Iterator).Value);
         VLQ_Vectors.Next (Iterator);
      end loop;
      return Result;
   end To_Int;

   Test : VLQ_Vectors.Vector;
begin
   Test := To_VLQ (16#7f#);
   Nat_IO.Put (To_Int (Test), 10, 16); Ada.Text_IO.Put (" = ");
   Print (Test);
   Test := To_VLQ (16#4000#);
   Nat_IO.Put (To_Int (Test), 10, 16); Ada.Text_IO.Put (" = ");
   Print (Test);
   Test := To_VLQ (16#0#);
   Nat_IO.Put (To_Int (Test), 10, 16); Ada.Text_IO.Put (" = ");
   Print (Test);
   Test := To_VLQ (16#3FFFFE#);
   Nat_IO.Put (To_Int (Test), 10, 16); Ada.Text_IO.Put (" = ");
   Print (Test);
   Test := To_VLQ (16#1FFFFF#);
   Nat_IO.Put (To_Int (Test), 10, 16); Ada.Text_IO.Put (" = ");
   Print (Test);
   Test := To_VLQ (16#200000#);
   Nat_IO.Put (To_Int (Test), 10, 16); Ada.Text_IO.Put (" = ");
   Print (Test);
end VLQ;
