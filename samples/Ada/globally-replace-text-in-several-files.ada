with Ada.Strings.Unbounded, Ada.Text_IO, Ada.Command_Line, Ada.Directories;

procedure Global_Replace is

   subtype U_String is Ada.Strings.Unbounded.Unbounded_String;
   function "+"(S: String) return U_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;
   function "-"(U: U_String) return String renames
     Ada.Strings.Unbounded.To_String;

   procedure String_Replace(S: in out U_String; Pattern, Replacement: String) is
      -- example: if S is "Mary had a XX lamb", then String_Replace(S, "X", "little");
      --          will turn S into "Mary had a littlelittle lamb"
      --          and String_Replace(S, "Y", "small"); will not change S

      Index : Natural;
   begin
      loop
         Index := Ada.Strings.Unbounded.Index(Source => S, Pattern => Pattern);
         exit when Index = 0;
         Ada.Strings.Unbounded.Replace_Slice
           (Source => S, Low => Index, High => Index+Pattern'Length-1,
            By => Replacement);
      end loop;
   end String_Replace;

   procedure File_Replace(Filename: String; Pattern, Replacement: String) is
      -- applies String_Rplace to each line in the file with the given Filename
      -- propagates any exceptions, when, e.g., the file does not exist

      I_File, O_File: Ada.Text_IO.File_Type;
      Line: U_String;
      Tmp_Name: String := Filename & ".tmp";
         -- name of temporary file; if that file already exists, it will be overwritten
   begin
      Ada.Text_IO.Open(I_File, Ada.Text_IO.In_File, Filename);
      Ada.Text_IO.Create(O_File, Ada.Text_IO.Out_File, Tmp_Name);
      while not Ada.Text_IO.End_Of_File(I_File) loop
         Line := +Ada.Text_IO.Get_Line(I_File);
         String_Replace(Line, Pattern, Replacement);
         Ada.Text_IO.Put_Line(O_File, -Line);
      end loop;
      Ada.Text_IO.Close(I_File);
      Ada.Text_IO.Close(O_File);
      Ada.Directories.Delete_File(Filename);
      Ada.Directories.Rename(Old_Name => Tmp_Name, New_Name => Filename);
   end File_Replace;

   Pattern:     String := Ada.Command_Line.Argument(1);
   Replacement: String :=  Ada.Command_Line.Argument(2);

begin
   Ada.Text_IO.Put_Line("Replacing """ & Pattern
                          & """ by """ & Replacement & """ in"
                          & Integer'Image(Ada.Command_Line.Argument_Count - 2)
                          & " files.");
   for I in 3 .. Ada.Command_Line.Argument_Count loop
      File_Replace(Ada.Command_Line.Argument(I), Pattern, Replacement);
   end loop;
end Global_Replace;
