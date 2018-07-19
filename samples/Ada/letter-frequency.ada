with Ada.Text_IO;

procedure Letter_Frequency is
   Counters: array (Character) of Natural := (others => 0); -- initialize all Counters to 0
   C:        Character;
   File:     Ada.Text_IO.File_Type;

begin
   Ada.Text_IO.Open(File, Mode => Ada.Text_IO.In_File, Name => "letter_frequency.adb");
   while not Ada.Text_IO.End_Of_File(File) loop
      Ada.Text_IO.Get(File, C);
      Counters(C) := Counters(C) + 1;
   end loop;

   for I in Counters'Range loop
      if Counters(I) > 0 then
            Ada.Text_IO.Put_Line("'" & I & "':" & Integer'Image(Counters(I)));
      end if;
   end loop;
end Letter_Frequency;
