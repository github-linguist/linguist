with Ada.Calendar.Arithmetic;
with Ada.Text_IO;
procedure Discordian is
   use Ada.Calendar;
   subtype Year_Number is Integer range 3067 .. 3565;
   type Seasons is (Chaos, Discord, Confusion, Bureaucracy, The_Aftermath);
   subtype Day_Number is Integer range 1 .. 73;

   type Discordian_Date is record
      Year        : Year_Number;
      Season      : Seasons;
      Day         : Day_Number;
      Is_Tibs_Day : Boolean := False;
   end record;

   procedure Convert (From : Time; To : out Discordian_Date) is
      use Ada.Calendar.Arithmetic;
      First_Day   : Time;
      Number_Days : Day_Count;
   begin
      First_Day   := Time_Of (Year => Year (From), Month => 1, Day => 1);
      Number_Days := From - First_Day;

      To.Year        := Year (Date => From) + 1166;
      To.Is_Tibs_Day := False;
      if (To.Year - 2) mod 4 = 0 then
         if Number_Days > 59 then
            Number_Days := Number_Days - 1;
         elsif Number_Days = 59 then
            To.Is_Tibs_Day := True;
         end if;
      end if;
      To.Day := Day_Number (Number_Days mod 73 + 1);
      case Number_Days / 73 is
         when 0 => To.Season := Chaos;
         when 1 => To.Season := Discord;
         when 2 => To.Season := Confusion;
         when 3 => To.Season := Bureaucracy;
         when 4 => To.Season := The_Aftermath;
         when others => raise Constraint_Error;
      end case;
   end Convert;

   procedure Put (Item : Discordian_Date) is
   begin
      Ada.Text_IO.Put ("YOLD" & Integer'Image (Item.Year));
      if Item.Is_Tibs_Day then
         Ada.Text_IO.Put (", St. Tib's Day");
      else
         Ada.Text_IO.Put (", " & Seasons'Image (Item.Season));
         Ada.Text_IO.Put (" " & Integer'Image (Item.Day));
      end if;
      Ada.Text_IO.New_Line;
   end Put;

   Test_Day  : Time;
   Test_DDay : Discordian_Date;
begin
   Test_Day := Clock;
   Convert (From => Test_Day, To => Test_DDay);
   Put (Test_DDay);
   Test_Day := Time_Of (Year => 2012, Month => 2, Day => 28);
   Convert (From => Test_Day, To => Test_DDay);
   Put (Test_DDay);
   Test_Day := Time_Of (Year => 2012, Month => 2, Day => 29);
   Convert (From => Test_Day, To => Test_DDay);
   Put (Test_DDay);
   Test_Day := Time_Of (Year => 2012, Month => 3, Day => 1);
   Convert (From => Test_Day, To => Test_DDay);
   Put (Test_DDay);
   Test_Day := Time_Of (Year => 2010, Month => 7, Day => 22);
   Convert (From => Test_Day, To => Test_DDay);
   Put (Test_DDay);
   Test_Day := Time_Of (Year => 2012, Month => 9, Day => 2);
   Convert (From => Test_Day, To => Test_DDay);
   Put (Test_DDay);
   Test_Day := Time_Of (Year => 2012, Month => 12, Day => 31);
   Convert (From => Test_Day, To => Test_DDay);
   Put (Test_DDay);
end Discordian;
