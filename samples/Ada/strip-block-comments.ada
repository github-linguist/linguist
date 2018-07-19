with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;

procedure Strip is
   use Ada.Strings.Unbounded;
   procedure Print_Usage is
   begin
      Ada.Text_IO.Put_Line ("Usage:");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("   strip <file> [<opening> [<closing>]]");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("      file: file to strip");
      Ada.Text_IO.Put_Line ("      opening: string for opening comment");
      Ada.Text_IO.Put_Line ("      closing: string for closing comment");
      Ada.Text_IO.New_Line;
   end Print_Usage;

   Opening_Pattern : Unbounded_String := To_Unbounded_String ("/*");
   Closing_Pattern : Unbounded_String := To_Unbounded_String ("*/");
   Inside_Comment  : Boolean          := False;

   function Strip_Comments (From : String) return String is
      use Ada.Strings.Fixed;
      Opening_Index : Natural;
      Closing_Index : Natural;
      Start_Index   : Natural := From'First;
   begin
      if Inside_Comment then
         Start_Index :=
            Index (Source => From, Pattern => To_String (Closing_Pattern));
         if Start_Index < From'First then
            return "";
         end if;
         Inside_Comment := False;
         Start_Index    := Start_Index + Length (Closing_Pattern);
      end if;
      Opening_Index :=
         Index
           (Source  => From,
            Pattern => To_String (Opening_Pattern),
            From    => Start_Index);
      if Opening_Index < From'First then
         return From (Start_Index .. From'Last);
      else
         Closing_Index :=
            Index
              (Source  => From,
               Pattern => To_String (Closing_Pattern),
               From    => Opening_Index + Length (Opening_Pattern));
         if Closing_Index > 0 then
            return From (Start_Index .. Opening_Index - 1) &
                   Strip_Comments
                      (From (
               Closing_Index + Length (Closing_Pattern) .. From'Last));
         else
            Inside_Comment := True;
            return From (Start_Index .. Opening_Index - 1);
         end if;
      end if;
   end Strip_Comments;

   File : Ada.Text_IO.File_Type;
begin
   if Ada.Command_Line.Argument_Count < 1
     or else Ada.Command_Line.Argument_Count > 3
   then
      Print_Usage;
      return;
   end if;
   if Ada.Command_Line.Argument_Count > 1 then
      Opening_Pattern := To_Unbounded_String (Ada.Command_Line.Argument (2));
      if Ada.Command_Line.Argument_Count > 2 then
         Closing_Pattern :=
            To_Unbounded_String (Ada.Command_Line.Argument (3));
      else
         Closing_Pattern := Opening_Pattern;
      end if;
   end if;
   Ada.Text_IO.Open
     (File => File,
      Mode => Ada.Text_IO.In_File,
      Name => Ada.Command_Line.Argument (1));
   while not Ada.Text_IO.End_Of_File (File => File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
      begin
         Ada.Text_IO.Put_Line (Strip_Comments (Line));
      end;
   end loop;
   Ada.Text_IO.Close (File => File);
end Strip;
