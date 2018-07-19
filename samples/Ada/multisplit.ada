with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Text_IO;

procedure Multisplit is
   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Element_Type => String);
   use type String_Lists.Cursor;

   function Split
     (Source     : String;
      Separators : String_Lists.List)
      return       String_Lists.List
   is
      Result             : String_Lists.List;
      Next_Position      : Natural := Source'First;
      Prev_Position      : Natural := Source'First;
      Separator_Position : String_Lists.Cursor;
      Separator_Length   : Natural;
      Changed            : Boolean;
   begin
      loop
         Changed            := False;
         Separator_Position := Separators.First;
         while Separator_Position /= String_Lists.No_Element loop
            Separator_Length :=
              String_Lists.Element (Separator_Position)'Length;
            if Next_Position + Separator_Length - 1 <= Source'Last
              and then Source
                (Next_Position .. Next_Position + Separator_Length - 1) =
                String_Lists.Element (Separator_Position)
            then
               if Next_Position > Prev_Position then
                  Result.Append
                    (Source (Prev_Position .. Next_Position - 1));
               end if;
               Result.Append (String_Lists.Element (Separator_Position));
               Next_Position := Next_Position + Separator_Length;
               Prev_Position := Next_Position;
               Changed       := True;
               exit;
            end if;
            Separator_Position := String_Lists.Next (Separator_Position);
         end loop;
         if not Changed then
            Next_Position := Next_Position + 1;
         end if;
         if Next_Position > Source'Last then
            Result.Append (Source (Prev_Position .. Source'Last));
            exit;
         end if;
      end loop;
      return Result;
   end Split;

   Test_Input      : constant String := "a!===b=!=c";
   Test_Separators : String_Lists.List;
   Test_Result     : String_Lists.List;
   Pos             : String_Lists.Cursor;
begin
   Test_Separators.Append ("==");
   Test_Separators.Append ("!=");
   Test_Separators.Append ("=");
   Test_Result := Split (Test_Input, Test_Separators);
   Pos         := Test_Result.First;
   while Pos /= String_Lists.No_Element loop
      Ada.Text_IO.Put (" " & String_Lists.Element (Pos));
      Pos := String_Lists.Next (Pos);
   end loop;
   Ada.Text_IO.New_Line;
   -- other order of separators
   Test_Separators.Clear;
   Test_Separators.Append ("=");
   Test_Separators.Append ("!=");
   Test_Separators.Append ("==");
   Test_Result := Split (Test_Input, Test_Separators);
   Pos         := Test_Result.First;
   while Pos /= String_Lists.No_Element loop
      Ada.Text_IO.Put (" " & String_Lists.Element (Pos));
      Pos := String_Lists.Next (Pos);
   end loop;
end Multisplit;
