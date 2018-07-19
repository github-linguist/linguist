with Aws.Client, Aws.Messages, Aws.Response, Aws.Resources, Aws.Url;
with Dom.Readers, Dom.Core, Dom.Core.Documents, Dom.Core.Nodes, Dom.Core.Attrs;
with Input_Sources.Strings, Unicode, Unicode.Ces.Utf8;
with Ada.Strings.Unbounded, Ada.Strings.Fixed, Ada.Text_IO, Ada.Command_Line;
with Ada.Containers.Vectors;

use  Aws.Client, Aws.Messages, Aws.Response, Aws.Resources, Aws.Url;
use Dom.Readers, Dom.Core, Dom.Core.Documents, Dom.Core.Nodes, Dom.Core.Attrs;
use Aws, Ada.Strings.Unbounded, Ada.Strings.Fixed, Input_Sources.Strings;
use Ada.Text_IO, Ada.Command_Line;

procedure Count_Examples is

   package Members_Vectors is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => Unbounded_String);
   use Members_Vectors;

   Exemples      : Vector;
   Nbr_Lg, Total : Natural := 0;

   procedure Get_Vector (Category : in String; Mbr_Vector : in out Vector) is
      Reader  : Tree_Reader;
      Doc     : Document;
      List    : Node_List;
      N       : Node;
      A       : Attr;
      Page    : Aws.Response.Data;
      Uri_Xml : constant String :=
         "http://rosettacode.org/mw/api.php?action=query&list=categorymembers"
         &
         "&format=xml&cmlimit=500&cmtitle=Category:";
   begin
      Page := Client.Get (Uri_Xml & Category);
      if Response.Status_Code (Page) not  in Messages.Success then
         raise Client.Connection_Error;
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
         Append (Mbr_Vector, To_Unbounded_String (Value (A)));
      end loop;
      Free (List);
      Free (Reader);
   end Get_Vector;

   function Scan_Page (Title : String) return Natural is
      Page                      : Aws.Response.Data;
      File                      : Aws.Resources.File_Type;
      Buffer                    : String (1 .. 1024);
      Languages, Position, Last : Natural := 0;
   begin
      Page :=
         Client.Get
           ("http://rosettacode.org/mw/index.php?title=" &
            Aws.Url.Encode (Title) &
            "&action=raw");
      Response.Message_Body (Page, File);
      while not End_Of_File (File) loop
         Resources.Get_Line (File, Buffer, Last);
         Position :=
            Index
              (Source  => Buffer (Buffer'First .. Last),
               Pattern => "=={{header|");
         if Position > 0 then
            Languages := Languages + 1;
         end if;
      end loop;
      Close (File);
      return Languages;
   end Scan_Page;

begin
   Get_Vector ("Programming_Tasks", Exemples);

   for I in First_Index (Exemples) .. Last_Index (Exemples) loop
      declare
         Title : constant String :=
            To_String (Members_Vectors.Element (Exemples, I));
      begin
         Nbr_Lg := Scan_Page (Title);
         Total  := Total + Nbr_Lg;
         Put_Line (Title & " :" & Integer'Image (Nbr_Lg) & " exemples.");
      end;
   end loop;

   Put_Line ("Total :" & Integer'Image (Total) & " exemples.");
end Count_Examples;
