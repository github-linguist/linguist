set fiveWeekendMonths to {}
set noFiveWeekendYears to {}

set someDate to current date
set day of someDate to 1

repeat with someYear from 1900 to 2100
  set year of someDate to someYear
  set foundOne to false
  repeat with someMonth in {January, March, May, July, ¬
                            August, October, December}
    set month of someDate to someMonth
    if weekday of someDate is Friday then
        set foundOne to true
        set end of fiveWeekendMonths to ¬
             (someYear as text) & "-" & (someMonth as text)
    end
  end repeat
  if not foundOne then
    set end of noFiveWeekendYears to someYear
  end
end repeat

set text item delimiters to ", "
set monthList to ¬
  (items 1 thru 5 of fiveWeekendMonths as text) & ", ..." & linefeed & ¬
   " ..., " & (items -5 thru end of fiveWeekendMonths as text)

set monthCount to count fiveWeekendMonths
set yearCount to count noFiveWeekendYears

set resultText to ¬
  "Months with five weekends (" & monthCount & "): " & linefeed & ¬
  "      " & monthList & linefeed & linefeed & ¬
  "Years with no such months (" & yearCount & "): "

set y to 1
repeat while y < yearCount
  set final to y+11
  if final > yearCount then
    set final to yearCount
  end
  set resultText to ¬
    resultText & linefeed & ¬
    "      " & (items y through final of noFiveWeekendYears as text)
  set y to y + 12
end
resultText
