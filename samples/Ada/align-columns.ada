with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Text_IO;             use Ada.Text_IO;
with Strings_Edit;            use Strings_Edit;

procedure Column_Aligner is
   Text : constant String :=
      "Given$a$text$file$of$many$lines,$where$fields$within$a$line$" & NUL &
      "are$delineated$by$a$single$'dollar'$character,$write$a$program" & NUL &
      "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$" & NUL &
      "column$are$separated$by$at$least$one$space." & NUL &
      "Further,$allow$for$each$word$in$a$column$to$be$either$left$" & NUL &
      "justified,$right$justified,$or$center$justified$within$its$column." & NUL;
   File    : File_Type;
   Width   : array (1..1_000) of Natural := (others => 0);
   Line    : String (1..200);
   Column  : Positive := 1;
   Start   : Positive := 1;
   Pointer : Positive;
begin
   Create (File, Out_File, "columned.txt");
      -- Determining the widths of columns
   for I in Text'Range loop
      case Text (I) is
         when '$' | NUL =>
            Width (Column) := Natural'Max (Width (Column), I - Start + 1);
            Start  := I + 1;
            if Text (I) = NUL then
               Column := 1;
            else
               Column := Column + 1;
            end if;
         when others =>
            null;
      end case;
   end loop;
      -- Formatting
   for Align in Alignment loop
      Column  := 1;
      Start   := 1;
      Pointer := 1;
      for I in Text'Range loop
         case Text (I) is
            when '$' | NUL =>
               Put -- Formatted output of a word
               (  Destination => Line,
                  Pointer     => Pointer,
                  Value       => Text (Start..I - 1),
                  Field       => Width (Column),
                  Justify     => Align
               );
               Start  := I + 1;
               if Text (I) = NUL then
                  Put_Line (File, Line (1..Pointer - 1));
                  Pointer := 1;
                  Column := 1;
               else
                  Column := Column + 1;
               end if;
            when others =>
               null;
         end case;
      end loop;
   end loop;
   Close (File);
end Column_Aligner;
