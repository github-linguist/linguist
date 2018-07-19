with AWS.Client, AWS.Response, AWS.Resources, AWS.Messages;
with Ada.Text_IO, Ada.Strings.Fixed;
use  Ada, AWS, AWS.Resources, AWS.Messages;

procedure Get_UTC_Time is

   Page           : Response.Data;
   File           : Resources.File_Type;
   Buffer         : String (1 .. 1024);
   Position, Last : Natural := 0;
   S              : Messages.Status_Code;
begin
   Page := Client.Get ("http://tycho.usno.navy.mil/cgi-bin/timer.pl");
   S    := Response.Status_Code (Page);
   if S not  in Success then
      Text_IO.Put_Line
        ("Unable to retrieve data => Status Code :" & Image (S) &
         " Reason :" & Reason_Phrase (S));
      return;
   end if;

   Response.Message_Body (Page, File);
   while not End_Of_File (File) loop
      Resources.Get_Line (File, Buffer, Last);
      Position :=
         Strings.Fixed.Index
           (Source  => Buffer (Buffer'First .. Last),
            Pattern => "UTC");
      if Position > 0 then
         Text_IO.Put_Line (Buffer (5 .. Position + 2));
         return;
      end if;
   end loop;
end Get_UTC_Time;
