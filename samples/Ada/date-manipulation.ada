with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Date_Manipulation is

   type Month_Name_T is
     (January, February, March, April, May, June,
      July, August, September, October, November, December);

   type Time_Zone_Name_T is (EST, Lisbon);

   type Period_T is (AM, PM);

   package TZ renames Ada.Calendar.Time_Zones;
   use type TZ.Time_Offset;

   Time_Zone_Offset : array (Time_Zone_Name_T) of TZ.Time_Offset :=
     (EST => -5 * 60,
      Lisbon => 0);

   Period_Offset : array (Period_T) of Natural :=
     (AM => 0,
      PM => 12);

   package Month_Name_IO is
      new Ada.Text_IO.Enumeration_IO (Month_Name_T);

   package Time_Zone_Name_IO is
      new Ada.Text_IO.Enumeration_IO (Time_Zone_Name_T);

   package Period_IO is
      new Ada.Text_IO.Enumeration_IO (Period_T);

   package Std renames Ada.Calendar;
   use type Std.Time;

   package Fmt renames Std.Formatting;

   function To_Number (Name : Month_Name_T) return Std.Month_Number is
   begin
      return Std.Month_Number (Month_Name_T'Pos (Name) + 1);
   end;

   function To_Time (S : String) return Std.Time is
      Month : Month_Name_T;
      Day : Std.Day_Number;
      Year : Std.Year_Number;
      Hour : Fmt.Hour_Number;
      Minute : Fmt.Minute_Number;
      Period : Period_T;
      Time_Zone : Time_Zone_Name_T;
      I : Natural;
   begin
      Month_Name_IO.Get
        (From => S, Item => Month, Last => I);
      Ada.Integer_Text_IO.Get
        (From => S (I + 1 .. S'Last), Item => Day, Last => I);
      Ada.Integer_Text_IO.Get
        (From => S (I + 1 .. S'Last), Item => Year, Last => I);
      Ada.Integer_Text_IO.Get
        (From => S (I + 1 .. S'Last), Item => Hour, Last => I);
      Ada.Integer_Text_IO.Get
        (From => S (I + 2 .. S'Last), Item => Minute, Last => I);
         --  here we start 2 chars down to skip the ':'
      Period_IO.Get
        (From => S (I + 1 .. S'Last), Item => Period, Last => I);
      Time_Zone_Name_IO.Get
        (From => S (I + 1 .. S'Last), Item => Time_Zone, Last => I);
      return Fmt.Time_Of
        (Year => Year,
         Month => To_Number (Month),
         Day => Day,
         Hour => Hour + Period_Offset (Period),
         Minute => Minute,
         Second => 0,
         Time_Zone => Time_Zone_Offset (Time_Zone));
   end;

   function Img
     (Date : Std.Time; Zone : Time_Zone_Name_T) return String is
   begin
      return
         Fmt.Image (Date => Date, Time_Zone => Time_Zone_Offset (Zone)) &
         " " & Time_Zone_Name_T'Image (Zone);
   end;

   T1, T2 : Std.Time;
   use Ada.Text_IO;
begin
   T1 := To_Time ("March 7 2009 7:30pm EST");
   T2 := T1 + 12.0 * 60.0 * 60.0;
   Put_Line ("T1 => " & Img (T1, EST) & " = " & Img (T1, Lisbon));
   Put_Line ("T2 => " & Img (T2, EST) & " = " & Img (T2, Lisbon));
end;
