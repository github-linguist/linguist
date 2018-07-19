with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
procedure Test_Run_Length_Encoding is
   function Encode (Data : String) return String is
   begin
      if Data'Length = 0 then
         return "";
      else
         declare
            Code  : constant Character := Data (Data'First);
            Index : Integer := Data'First + 1;
         begin
            while Index <= Data'Last and then Code = Data (Index) loop
               Index := Index + 1;
            end loop;
            declare
               Prefix : constant String := Integer'Image (Index - Data'First);
            begin
               return Prefix (2..Prefix'Last) & Code & Encode (Data (Index..Data'Last));
            end;
         end;
      end if;
   end Encode;
   function Decode (Data : String) return String is
   begin
      if Data'Length = 0 then
         return "";
      else
         declare
            Index : Integer := Data'First;
            Count : Natural := 0;
         begin
            while Index < Data'Last and then Data (Index) in '0'..'9' loop
               Count := Count * 10 + Character'Pos (Data (Index)) - Character'Pos ('0');
               Index := Index + 1;
            end loop;
            if Index > Data'First then
               return Count * Data (Index) & Decode (Data (Index + 1..Data'Last));
            else
               return Data;
            end if;
         end;
      end if;
   end Decode;
begin
   Put_Line (Encode ("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"));
   Put_Line (Decode ("12W1B12W3B24W1B14W"));
end Test_Run_Length_Encoding;
