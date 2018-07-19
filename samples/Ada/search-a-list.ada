with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Text_IO;            use Ada.Text_IO;

procedure Test_List_Index is
   Not_In : exception;

   type List is array (Positive range <>) of Unbounded_String;

   function Index (Haystack : List; Needle : String) return Positive is
   begin
      for Index in Haystack'Range loop
         if Haystack (Index) = Needle then
            return Index;
         end if;
      end loop;
      raise Not_In;
   end Index;

      -- Functions to create lists
   function "+" (X, Y : String) return List is
   begin
      return (1 => To_Unbounded_String (X), 2 => To_Unbounded_String (Y));
   end "+";

   function "+" (X : List; Y : String) return List is
   begin
      return X & (1 => To_Unbounded_String (Y));
   end "+";

   Haystack : List := "Zig"+"Zag"+"Wally"+"Ronald"+"Bush"+"Krusty"+"Charlie"+"Bush"+"Bozo";

   procedure Check (Needle : String) is
   begin
      Put (Needle);
      Put_Line ("at" & Positive'Image (Index (Haystack, Needle)));
   exception
      when Not_In => Put_Line (" is not in");
   end Check;
begin
   Check ("Washington");
   Check ("Bush");
end Test_List_Index;
