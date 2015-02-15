with Ada.Strings.Fixed, Ada.Text_IO;

procedure Count_Substrings is

   function Substrings(Main: String; Sub: String) return Natural is
      Idx: Natural :=  Ada.Strings.Fixed.Index(Source => Main, Pattern => Sub);
   begin
      if Idx = 0 then
         return 0;
      else
         return 1 + Substrings(Main(Idx+Sub'Length .. Main'Last), Sub);
      end if;
   end Substrings;

begin
   Ada.Text_IO.Put(Integer'Image(Substrings("the three truths", "th")));
   Ada.Text_IO.Put(Integer'Image(Substrings("ababababab", "abab")));
end Count_Substrings;
