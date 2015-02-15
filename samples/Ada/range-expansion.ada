with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Range_Expansion is
   type Sequence is array (Positive range <>) of Integer;
   function Expand (Text : String) return Sequence is
      To    : Integer := Text'First;
      Count : Natural := 0;
      Low   : Integer;
      function Get return Integer is
         From : Integer := To;
      begin
         if Text (To) = '-' then
            To := To + 1;
         end if;
         while To <= Text'Last loop
            case Text (To) is
               when ',' | '-' => exit;
               when others => To := To + 1;
            end case;
         end loop;
         return Integer'Value (Text (From..To - 1));
      end Get;
   begin
      while To <= Text'Last loop -- Counting items of the list
         Low := Get;
         if To > Text'Last or else Text (To) = ',' then
            Count := Count + 1;
         else
            To := To + 1;
            Count := Count + Get - Low + 1;
         end if;
         To := To + 1;
      end loop;
      return Result : Sequence (1..Count) do
         Count := 0;
         To := Text'First;
         while To <= Text'Last loop -- Filling the list
            Low := Get;
            if To > Text'Last or else Text (To) = ',' then
               Count := Count + 1;
               Result (Count) := Low;
            else
               To := To + 1;
               for Item in Low..Get loop
                  Count := Count + 1;
                  Result (Count) := Item;
               end loop;
            end if;
            To := To + 1;
         end loop;
      end return;
   end Expand;
   procedure Put (S : Sequence) is
      First : Boolean := True;
   begin
      for I in S'Range loop
         if First then
            First := False;
         else
            Put (',');
         end if;
         Put (Integer'Image (S (I)));
      end loop;
   end Put;
begin
   Put (Expand ("-6,-3--1,3-5,7-11,14,15,17-20"));
end Test_Range_Expansion;
