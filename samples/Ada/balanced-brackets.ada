with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Ada.Strings.Fixed;
procedure Brackets is
   package Random_Positive is new Ada.Numerics.Discrete_Random (Positive);
   Positive_Generator : Random_Positive.Generator;
   procedure Swap (Left, Right : in out Character) is
      Temp : constant Character := Left;
   begin
      Left := Right;
      Right := Temp;
   end Swap;
   function Generate_Brackets (Bracket_Count : Natural;
                               Opening_Bracket : Character := '[';
                               Closing_Bracket : Character := ']')
            return String is
      use Ada.Strings.Fixed;
      All_Brackets : String := Bracket_Count * Opening_Bracket & Bracket_Count * Closing_Bracket;
   begin
      for I in All_Brackets'Range loop
         Swap (All_Brackets (I), All_Brackets (Random_Positive.Random (Positive_Generator) mod (Bracket_Count * 2) + 1));
      end loop;
      return All_Brackets;
   end Generate_Brackets;

   function Check_Brackets (Test : String;
                            Opening_Bracket : Character := '[';
                            Closing_Bracket : Character := ']')
            return Boolean is
      Open : Natural := 0;
   begin
      for I in Test'Range loop
         if Test (I) = Opening_Bracket then
            Open := Open + 1;
         elsif Test (I) = Closing_Bracket then
            if Open = 0 then
               return False;
            else
               Open := Open - 1;
            end if;
         end if;
      end loop;
      return True;
   end Check_Brackets;
begin
   Random_Positive.Reset (Positive_Generator);
   Ada.Text_IO.Put_Line ("Brackets");
   for I in 0 .. 4 loop
      for J in 0 .. I loop
         declare
            My_String : constant String := Generate_Brackets (I);
         begin
            Ada.Text_IO.Put_Line (My_String & ": " & Boolean'Image (Check_Brackets (My_String)));
         end;
      end loop;
   end loop;
end Brackets;
