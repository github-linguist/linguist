with Ada.Characters.Latin_1;
with Ada.Streams.Stream_IO;  use Ada.Streams.Stream_IO;

procedure Put_PPM (File : File_Type; Picture : Image) is
   use Ada.Characters.Latin_1;
   Size   : constant String := Integer'Image (Picture'Length (2)) & Integer'Image (Picture'Length (1));
   Buffer : String (1..Picture'Length (2) * 3);
   Color  : Pixel;
   Index  : Positive;
begin
   String'Write (Stream (File), "P6" & LF);
   String'Write (Stream (File), Size (2..Size'Last) & LF);
   String'Write (Stream (File), "255" & LF);
   for I in Picture'Range (1) loop
      Index := Buffer'First;
      for J in Picture'Range (2) loop
         Color := Picture (I, J);
         Buffer (Index)     := Character'Val (Color.R);
         Buffer (Index + 1) := Character'Val (Color.G);
         Buffer (Index + 2) := Character'Val (Color.B);
         Index := Index + 3;
      end loop;
      String'Write (Stream (File), Buffer);
   end loop;
   Character'Write (Stream (File), LF);
end Put_PPM;
