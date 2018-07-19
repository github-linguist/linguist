with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Strings.Less_Case_Insensitive;

with AWS.Client;
with AWS.Response;

procedure Test is

   use Ada.Strings;

   function "+" (S : String) return Unbounded_String renames To_Unbounded_String;

   type A_Language_Count is
      record
         Count    : Integer := 0;
         Language : Unbounded_String;
      end record;

   function "=" (L, R : A_Language_Count) return Boolean is
   begin
      return L.Language = R.Language;
   end "=";

   function "<" (L, R : A_Language_Count) return Boolean is
   begin
      -- Sort by 'Count' and then by Language name
      return L.Count < R.Count
        or else (L.Count = R.Count
                 and then Less_Case_Insensitive (Left  => To_String (L.Language),
                                                 Right => To_String (R.Language)));
   end "<";

   package Sets is new Ada.Containers.Ordered_Sets (A_Language_Count);
   use Sets;

   Counts : Set;

   procedure Find_Counts (S : String) is
      Title_Str : constant String  := "title=""Category:";
      End_A_Str : constant String  := "</a> (";

      Title_At   : constant Natural := Index (S, Title_Str);
   begin
      if Title_At /= 0 then
         declare
            Bracket_At : constant Natural := Index (S (Title_At   + Title_Str'Length .. S'Last), ">");
            End_A_At   : constant Natural := Index (S (Bracket_At + 1                .. S'Last), End_A_Str);
            Space_At   : constant Natural := Index (S (End_A_At   + End_A_Str'Length .. S'Last), " ");
            Count      : constant Natural := Natural'Value (S (End_A_At + End_A_Str'Length .. Space_At - 1));
            Language   : constant String  :=                S (Title_At + Title_Str'Length .. Bracket_At - 2);
         begin
            if Bracket_At /= 0 and then End_A_At /= 0 and then Space_At /= 0 then
               begin
                  Counts.Insert (New_Item => (Count, +Language));
               exception
                  when Constraint_Error =>
                     Put_Line (Standard_Error, "Warning: repeated language: " & Language);
                     -- Ignore repeated results.
                     null;
               end;
            end if;
            -- Recursively parse the string for languages and counts
            Find_Counts (S (Space_At + 1 .. S'Last));
         end;
      end if;

   end Find_Counts;

   Place : Natural := 1;

   procedure Display (C : Cursor) is
   begin
      Put (Place, Width => 1);             Put (". ");
      Put (Element (C).Count, Width => 1); Put (" - ");
      Put_Line (To_String (Element (C).Language));
      Place := Place + 1;
   end Display;

   Http_Source : constant AWS.Response.Data :=
     AWS.Client.Get ("http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000");
begin
   Find_Counts (AWS.Response.Message_Body (Http_Source));
   Counts.Reverse_Iterate (Display'Access);
end Test;
