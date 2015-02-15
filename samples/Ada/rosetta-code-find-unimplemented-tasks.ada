with Aws.Client, Aws.Messages, Aws.Response, Aws.Resources, Aws.Url;
with Dom.Readers, Dom.Core, Dom.Core.Documents, Dom.Core.Nodes, Dom.Core.Attrs;
with Input_Sources.Strings, Unicode, Unicode.Ces.Utf8;
with Ada.Strings.Unbounded, Ada.Text_IO, Ada.Command_Line;
with Ada.Containers.Vectors;

use  Aws.Client, Aws.Messages, Aws.Response, Aws.Resources, Aws.Url;
use Dom.Readers, Dom.Core, Dom.Core.Documents, Dom.Core.Nodes, Dom.Core.Attrs;
use Aws, Ada.Strings.Unbounded, Input_Sources.Strings;
use Ada.Text_IO, Ada.Command_Line;

procedure Not_Coded is

   package Members_Vectors is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => Unbounded_String);
   use Members_Vectors;

   All_Tasks, Language_Members : Vector;

   procedure Get_Vector (Category : in String; Mbr_Vector : in out Vector) is
      Reader : Tree_Reader;
      Doc    : Document;
      List   : Node_List;
      N      : Node;
      A      : Attr;
      Page   : Aws.Response.Data;
      S      : Messages.Status_Code;

      -- Query has cmlimit value of 100, so we need 5 calls to
      -- retrieve the complete list of Programming_category
      Uri_Xml    : constant String  :=
         "http://rosettacode.org/mw/api.php?action=query&list=categorymembers"
         &
         "&format=xml&cmlimit=100&cmtitle=Category:";
      Cmcontinue : Unbounded_String := Null_Unbounded_String;
   begin
      loop
         Page :=
            Aws.Client.Get (Uri_Xml & Category & (To_String (Cmcontinue)));
         S    := Response.Status_Code (Page);
         if S not  in Messages.Success then
            Put_Line
              ("Unable to retrieve data => Status Code :" &
               Image (S) &
               " Reason :" &
               Reason_Phrase (S));
            raise Connection_Error;
         end if;
         declare
            Xml    : constant String := Message_Body (Page);
            Source : String_Input;
         begin
            Open
              (Xml'Unrestricted_Access,
               Unicode.Ces.Utf8.Utf8_Encoding,
               Source);
            Parse (Reader, Source);
            Close (Source);
         end;
         Doc  := Get_Tree (Reader);
         List := Get_Elements_By_Tag_Name (Doc, "cm");
         for Index in 1 .. Length (List) loop
            N := Item (List, Index - 1);
            A := Get_Named_Item (Attributes (N), "title");
            Members_Vectors.Append
              (Mbr_Vector,
               To_Unbounded_String (Value (A)));
         end loop;
         Free (List);
         List := Get_Elements_By_Tag_Name (Doc, "query-continue");
         if Length (List) = 0 then
            -- we are done
            Free (List);
            Free (Reader);
            exit;
         end if;
         N          := First_Child (Item (List, 0));
         A          := Get_Named_Item (Attributes (N), "cmcontinue");
         Cmcontinue :=
            To_Unbounded_String
              ("&cmcontinue=" & Aws.Url.Encode (Value (A)));
         Free (List);
         Free (Reader);
      end loop;
   end Get_Vector;

   procedure Quick_Diff (From : in out Vector; Substract : in Vector) is
      Beginning, Position : Extended_Index;
   begin
      -- take adavantage that both lists are already sorted
      Beginning := First_Index (From);
      for I in First_Index (Substract) .. Last_Index (Substract) loop
         Position :=
            Find_Index
              (Container => From,
               Item      => Members_Vectors.Element (Substract, I),
               Index     => Beginning);
         if not (Position = No_Index) then
            Delete (From, Position);
            Beginning := Position;
         end if;
      end loop;
   end Quick_Diff;

begin
   if Argument_Count = 0 then
      Put_Line ("Can't process : No language given!");
      return;
   else
      Get_Vector (Argument (1), Language_Members);
   end if;

   Get_Vector ("Programming_Tasks", All_Tasks);
   Quick_Diff (All_Tasks, Language_Members);

   for I in First_Index (All_Tasks) .. Last_Index (All_Tasks) loop
      Put_Line (To_String (Members_Vectors.Element (All_Tasks, I)));
   end loop;
   Put_Line
     ("Numbers of tasks not implemented :=" &
      Integer'Image (Last_Index ((All_Tasks))));
end Not_Coded;
