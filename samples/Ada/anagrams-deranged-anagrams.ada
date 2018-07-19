with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Indefinite_Vectors;
procedure Danagrams is
   package StringVector is new Ada.Containers.Indefinite_Vectors
      (Positive, String);
   procedure StrSort is new Ada.Containers.Generic_Array_Sort
      (Index_Type => Positive,
      Element_Type => Character,
      Array_Type => String);
   function Derange (s1 : String; s2 : String) return Boolean is begin
      for i in s1'Range loop
         if (s1 (i) = s2 (i)) then return False; end if;
      end loop;
      return True;
   end Derange;
   File : File_Type;
   len, foundlen : Positive := 1;
   Vect, SVect : StringVector.Vector;
   index, p1, p2 : StringVector.Extended_Index := 0;
begin
   Open (File, In_File, "unixdict.txt");
   while not End_Of_File (File) loop
      declare str : String := Get_Line (File);
      begin
         len := str'Length;
         if len > foundlen then
            Vect.Append (str);
            StrSort (str);
            index := 0;
            loop --  Loop through anagrams by index in vector of sorted strings
               index := SVect.Find_Index (str, index + 1);
               exit when index = StringVector.No_Index;
               if Derange (Vect.Last_Element, Vect.Element (index)) then
                     p1 := Vect.Last_Index; p2 := index;
                     foundlen := len;
               end if;
            end loop;
            SVect.Append (str);
         end if;
      end;
   end loop;
   Close (File);
   Put_Line (Vect.Element (p1) & " " & Vect.Element (p2));
end Danagrams;
