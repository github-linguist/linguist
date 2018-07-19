with Ada.Text_IO;  use Ada.Text_IO;
with System;       use System;

procedure Host_Introspection is
begin
   Put_Line ("Word size" & Integer'Image (Word_Size));
   Put_Line ("Endianness " & Bit_Order'Image (Default_Bit_Order));
end Host_Introspection;
