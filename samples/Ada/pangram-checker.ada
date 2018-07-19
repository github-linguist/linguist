with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Characters.Handling; use Ada.Characters.Handling;
procedure pangram is

	function ispangram(txt: String) return Boolean is
		lowtxt : String := To_Lower(txt);
		letset,txtset : Character_Set;
		begin
		letset := To_Set("abcdefghijklmnopqrstuvwxyz");
		txtset := To_Set(lowtxt);
		return (letset-txtset)=Null_Set;
	end ispangram;

begin
put_line(Boolean'Image(ispangram("This is a test")));
put_line(Boolean'Image(ispangram("The quick brown fox jumps over the lazy dog")));
put_line(Boolean'Image(ispangram("NOPQRSTUVWXYZ  abcdefghijklm")));
put_line(Boolean'Image(ispangram("abcdefghijklopqrstuvwxyz"))); --Missing m, n
end pangram;
