Year := 1900
End_Year := 2100
31_Day_Months = 01,03,05,07,08,10,12

While Year <= End_Year
  {
    Loop, Parse, 31_Day_Months, CSV
      {
        FormatTime, Day, %Year%%A_LoopField%01, dddd
        IfEqual, Day, Friday
          {
            All_Months_With_5_Weekends .=  A_LoopField . "/" . Year ",    "
            5_Weekend_Count++
            Year_Has_5_Weekend_Month := 1
          }
      }
   IfEqual, Year_Has_5_Weekend_Month, 0
      {
        All_Years_Without_5_Weekend .= Year ",   "
        No_5_Weekend_Count ++
      }
   Year ++
   Year_Has_5_Weekend_Month := 0
  }
; Trim the spaces and comma off the last item.
StringTrimRight, All_Months_With_5_Weekends, All_Months_With_5_Weekends, 5
StringTrimRight, All_Years_Without_5_Weekend, All_Years_Without_5_Weekend, 4
MsgBox,
(
Months with 5 day weekends between 1900 and 2100 : %5_Weekend_Count%
%All_Months_With_5_Weekends%
)
MsgBox,
(
Years with no 5 day weekends between 1900 and 2100 : %No_5_Weekend_Count%
%All_Years_Without_5_Weekend%
)
