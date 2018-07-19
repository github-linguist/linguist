with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Text_Io; use Ada.Text_Io;

procedure System_Time is
   Now : Time := Clock;
begin
   Put_line(Image(Date => Now, Time_Zone => -7*60));
end System_Time;
