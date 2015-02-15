with Ada.Text_IO, Interfaces;
use Ada.Text_IO, Interfaces;

procedure Gray is

   Bits : constant := 5; -- Change only this line for 6 or 7-bit encodings
   subtype Values is Unsigned_8 range 0 .. 2 ** Bits - 1;
   package Values_Io is new Ada.Text_IO.Modular_IO (Values);

   function Encode (Binary : Values) return Values is
   begin
      return Binary xor Shift_Right (Binary, 1);
   end Encode;
   pragma Inline (Encode);

   function Decode (Gray : Values) return Values is
      Binary, Bit : Values;
      Mask        : Values := 2 ** (Bits - 1);
   begin
      Bit    := Gray and Mask;
      Binary := Bit;
      for I in 2 .. Bits loop
         Bit    := Shift_Right (Bit, 1);
         Mask   := Shift_Right (Mask, 1);
         Bit    := (Gray and Mask) xor Bit;
         Binary := Binary + Bit;
      end loop;
      return Binary;
   end Decode;
   pragma Inline (Decode);

   HT : constant Character := Character'Val (9);
   J  : Values;
begin
   Put_Line ("Num" & HT & "Binary" & HT & HT & "Gray" & HT & HT & "decoded");
   for I in Values'Range loop
      J := Encode (I);
      Values_Io.Put (I, 4);
      Put (": " & HT);
      Values_Io.Put (I, Bits + 2, 2);
      Put (" =>" & HT);
      Values_Io.Put (J, Bits + 2, 2);
      Put (" => " & HT);
      Values_Io.Put (Decode (J), 4);
      New_Line;
   end loop;
end Gray;
