with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
procedure Soundex is
   type UStrings is array(Natural range <>) of Unbounded_String;
   function "+"(S:String) return Unbounded_String renames To_Unbounded_String;

   function toSoundex (instr : String) return String is
      str : String := To_Upper(instr);
      output : String := "0000";
      spos : Integer := str'First+1;  opos : Positive := 2;
      map : array(0..255) of Character := (others => ' ');
      last : Integer := str'First;
   begin
      map(65..90) := " 123 12- 22455 12623 1-2 2";
      for i in str'Range loop str(i) := map(Character'Pos(str(i))); end loop;
      output(1) := str(str'First);
      while (opos <= 4 and spos <= str'Last) loop
         if str(spos) /= '-' and str(spos) /= ' ' then
            if (str(spos-1) = '-' and last = spos-2) and then
              (str(spos) = str(spos-2)) then null;
            elsif (str(spos) = output(opos-1) and last = spos-1) then last := spos;
            else output(opos) := str(spos);  opos := opos + 1; last := spos;
            end if;
         end if;
         spos := spos + 1;
      end loop;
      output(1) := To_Upper(instr(instr'First));
      return output;
   end toSoundex;

   cases : constant UStrings := (+"Soundex", +"Example", +"Sownteks",
      +"Ekzampul", +"Euler", +"Gauss", +"Hilbert", +"Knuth", +"Lloyd",
      +"Lukasiewicz", +"Ellery", +"Ghosh", +"Heilbronn", +"Kant",
      +"Ladd", +"Lissajous", +"Wheaton", +"Burroughs", +"Burrows",
      +"O'Hara", +"Washington", +"Lee", +"Gutierrez", +"Pfister",
      +"Jackson", +"Tymczak", +"VanDeusen", +"Ashcraft");
begin
   for i in cases'Range loop
      Put_Line(To_String(cases(i))&" = "&toSoundex(To_String(cases(i))));
   end loop;
end Soundex;
