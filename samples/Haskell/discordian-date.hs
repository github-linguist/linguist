import Data.List
import Data.Time
import Data.Time.Calendar.MonthDay

seasons = words "Chaos Discord Confusion Bureaucracy The_Aftermath"

discordianDate (y,m,d) = do
  let doy = monthAndDayToDayOfYear (isLeapYear y) m d
      (season, dday) = divMod doy 73
      dos = dday - fromEnum (isLeapYear y && m >2)
      dDate
	| isLeapYear y && m==2 && d==29 = "St. Tib's Day, " ++ show (y+1166) ++ " YOLD"
	| otherwise = seasons!!season ++ " " ++ show dos ++ ", " ++ show (y+1166) ++ " YOLD"
  putStrLn dDate
