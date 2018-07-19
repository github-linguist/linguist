with Ada.Text_IO; with Gnat.Regpat; use Ada.Text_IO;

procedure Regex is

   package Pat renames Gnat.Regpat;

   procedure Search_For_Pattern(Compiled_Expression: Pat.Pattern_Matcher;
                                Search_In: String;
                                First, Last: out Positive;
                                Found: out Boolean) is
      Result: Pat.Match_Array (0 .. 1);
   begin
      Pat.Match(Compiled_Expression, Search_In, Result);
      Found := not Pat."="(Result(1), Pat.No_Match);
      if Found then
         First := Result(1).First;
         Last := Result(1).Last;
      end if;
   end Search_For_Pattern;

   Word_Pattern: constant String := "([a-zA-Z]+)";

   Str:           String:= "I love PATTERN matching!";
   Current_First: Positive := Str'First;
   First, Last:   Positive;
   Found:         Boolean;

begin
   -- first, find all the words in Str
   loop
      Search_For_Pattern(Pat.Compile(Word_Pattern),
                         Str(Current_First .. Str'Last),
                         First, Last, Found);
   exit when not Found;
      Put_Line("<" & Str(First .. Last) & ">");
      Current_First := Last+1;
   end loop;

   -- second, replace "PATTERN" in Str by "pattern"
   Search_For_Pattern(Pat.Compile("(PATTERN)"), Str, First, Last, Found);
   Str := Str(Str'First .. First-1) & "pattern" & Str(Last+1 .. Str'Last);
   Put_Line(Str);
end Regex;
