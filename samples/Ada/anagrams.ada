with Ada.Text_IO;  use Ada.Text_IO;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

procedure Words_Of_Equal_Characters is
   package Set_Of_Words is new Ada.Containers.Indefinite_Ordered_Sets (String);
   use Ada.Containers, Set_Of_Words;
   package Anagrams is new Ada.Containers.Indefinite_Ordered_Maps (String, Set);
   use Anagrams;

   File   : File_Type;
   Result : Map;
   Max    : Count_Type := 1;

   procedure Put (Position : Anagrams.Cursor) is
      First : Boolean := True;
      List  : Set renames Element (Position);
      procedure Put (Position : Set_Of_Words.Cursor) is
      begin
         if First then
            First := False;
         else
            Put (',');
         end if;
         Put (Element (Position));
      end Put;
   begin
      if List.Length = Max then
         Iterate (List, Put'Access);
         New_Line;
      end if;
   end Put;

begin
   Open (File, In_File, "unixdict.txt");
   loop
      declare
         Word : constant String     := Get_Line (File);
         Key  : String (Word'Range) := (others => Character'Last);
         List : Set;
         Position : Anagrams.Cursor;
      begin
         for I in Word'Range loop
            for J in Word'Range loop
               if Key (J) > Word (I) then
                  Key (J + 1..I) := Key (J..I - 1);
                  Key (J) := Word (I);
                  exit;
               end if;
            end loop;
         end loop;
         Position := Find (Result, Key);
         if Has_Element (Position) then
            List := Element (Position);
            Insert (List, Word);
            Replace_Element (Result, Position, List);
         else
            Insert (List, Word);
            Include (Result, Key, List);
         end if;
         Max := Count_Type'Max (Max, Length (List));
      end;
   end loop;
exception
   when End_Error =>
      Iterate (Result, Put'Access);
      Close (File);
end Words_Of_Equal_Characters;
