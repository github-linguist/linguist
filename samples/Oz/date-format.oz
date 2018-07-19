declare
  WeekDays = unit(0:"Sunday" "Monday" "Tuesday" "Wednesday"
                  "Thursday" "Friday" "Saturday")
  Months = unit(0:"January" "February" "March" "April"
                "May" "June" "July" "August" "September"
                "October" "November" "December")

  fun {DateISO Time}
     Year = 1900 + Time.year
     Month = Time.mon + 1
  in
     Year#"-"#{Align Month}#"-"#{Align Time.mDay}
  end

  fun {DateLong Time}
     Year = 1900 + Time.year
  in
     WeekDays.(Time.wDay)#", "#Months.(Time.mon)#" "#Time.mDay#", "#Year
  end

  fun {Align Num}
     if Num < 10 then "0"#Num else Num end
  end
in
  {System.showInfo {DateISO {OS.localTime}}}
  {System.showInfo {DateLong {OS.localTime}}}
