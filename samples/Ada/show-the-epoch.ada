with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Conversions; use Ada.Calendar.Conversions;
procedure ShowEpoch is
   etime : Time := To_Ada_Time (0);
begin
   Put_Line (Image (Date => etime));
end ShowEpoch;
