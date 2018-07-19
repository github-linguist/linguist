with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_Io; use Ada.Text_Io;

procedure Parse_Commas is
   Source_String : String := "Hello,How,Are,You,Today";
   Index_List : array(Source_String'Range) of Natural;
   Next_Index : Natural := Index_List'First;
begin
   Index_List(Next_Index) := Source_String'First;
   while Index_List(Next_Index) < Source_String'Last loop
      Next_Index := Next_Index + 1;
      Index_List(Next_Index) := 1 + Index(Source_String(Index_List(Next_Index - 1)..Source_String'Last), ",");
      if Index_List(Next_Index) = 1 then
         Index_List(Next_Index) := Source_String'Last + 2;
      end if;
      Put(Source_String(Index_List(Next_Index - 1)..Index_List(Next_Index)-2) & ".");
   end loop;
end Parse_Commas;
