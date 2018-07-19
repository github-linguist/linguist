with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;
procedure Ordered_Words is
   package Word_Vectors is new Ada.Containers.Indefinite_Vectors
      (Index_Type => Positive, Element_Type => String);

   function Is_Ordered (The_Word : String) return Boolean is
      Highest_Character : Character := 'a';
   begin
      for I in The_Word'Range loop
         if The_Word(I) not in 'a' .. 'z' then
            return False;
         end if;
         if The_Word(I) < Highest_Character then
            return False;
         end if;
         Highest_Character := The_Word(I);
      end loop;
      return True;
   end Is_Ordered;

   procedure Print_Word (Position : Word_Vectors.Cursor) is
   begin
      Ada.Text_IO.Put_Line (Word_Vectors.Element (Position));
   end Print_Word;

   File : Ada.Text_IO.File_Type;
   Ordered_Words : Word_Vectors.Vector;
   Max_Length : Positive := 1;
begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "unixdict.txt");
   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Next_Word : String := Ada.Text_IO.Get_Line (File);
      begin
         if Is_Ordered (Next_Word) then
            if Next_Word'Length > Max_Length then
               Max_Length := Next_Word'Length;
               Word_Vectors.Clear (Ordered_Words);
               Word_Vectors.Append (Ordered_Words, Next_Word);
            elsif Next_Word'Length = Max_Length then
               Word_Vectors.Append (Ordered_Words, Next_Word);
            end if;
         end if;
      end;
   end loop;
   Word_Vectors.Iterate (Ordered_Words, Print_Word'Access);
end Ordered_Words;
