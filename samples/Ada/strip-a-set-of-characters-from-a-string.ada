with Ada.Text_IO;

procedure Strip_Characters_From_String is

   function Strip(The_String: String; The_Characters: String)
                  return String is
      Keep:   array (Character) of Boolean := (others => True);
      Result: String(The_String'Range);
      Last:   Natural := Result'First-1;
   begin
      for I in The_Characters'Range loop
         Keep(The_Characters(I)) := False;
      end loop;
      for J in The_String'Range loop
         if Keep(The_String(J)) then
            Last := Last+1;
            Result(Last) := The_String(J);
         end if;
      end loop;
      return Result(Result'First .. Last);
   end Strip;

   S: String := "She was a soul stripper. She took my heart!";

begin -- main
   Ada.Text_IO.Put_Line(Strip(S, "aei"));
end Strip_Characters_From_String;
