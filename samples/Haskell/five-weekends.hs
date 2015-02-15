import Data.List (intercalate)

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday |
    Saturday | Sunday
    deriving (Eq, Show)

-- the whole thing bases upon an infinite list of weeks

daysFrom1_1_1900 :: [DayOfWeek]
daysFrom1_1_1900 = concat $ repeat [Monday, Tuesday, Wednesday,
    Thursday, Friday, Saturday, Sunday]

data Month = January | February | March | April | May | June | July |
    August | September | October | November | December
    deriving (Show)

type Year = Int
type YearCalendar = (Year, [DayOfWeek])
type MonthlyCalendar = (Year, [(Month, [DayOfWeek])])

-- makes groups of 365 or 366 days for each year (infinite list)

yearsFrom :: [DayOfWeek] -> Year -> [YearCalendar]
yearsFrom s i = (i, yeardays) : yearsFrom rest (i + 1)
    where
        yeardays = take (leapOrNot i) s
        yearlen  = length yeardays
        rest    = drop yearlen s
        leapOrNot n = if isLeapYear n then 366 else 365

yearsFrom1900 :: [YearCalendar]
yearsFrom1900 = yearsFrom daysFrom1_1_1900 1900

-- makes groups of days for each month of the year

months :: YearCalendar -> MonthlyCalendar
months (y, d) = (y, [(January, january), (February, february),
    (March, march), (April, april), (May, may), (June, june),
    (July, july), (August, august), (September, september),
    (October, october), (November, november), (December, december)])
    where
        leapOrNot = if isLeapYear y then 29 else 28
        january = take 31 d
        february = take leapOrNot $ drop 31 d
        march = take 31 $ drop (31 + leapOrNot) d
        april = take 30 $ drop (62 + leapOrNot) d
        may = take 31 $ drop (92 + leapOrNot) d
        june = take 30 $ drop (123 + leapOrNot) d
        july = take 31 $ drop (153 + leapOrNot) d
        august = take 31 $ drop (184 + leapOrNot) d
        september = take 30 $ drop (215 + leapOrNot) d
        october = take 31 $ drop (245 + leapOrNot) d
        november = take 30 $ drop (276 + leapOrNot) d
        december = take 31 $ drop (306 + leapOrNot) d

-- see if a year is a leap year

isLeapYear n
    | n `mod` 100 == 0 = n `mod` 400 == 0
    | otherwise = n `mod` 4 == 0

-- make a list of the months of a year that have 5 weekends
-- (they must have 31 days and the first day must be Friday)
-- if the year doesn't contain any 5-weekended months, then
-- return the year and an empty list

whichFiveWeekends :: MonthlyCalendar -> (Year, [Month])
whichFiveWeekends (y, ms) = (y, map (\(m, _) -> m) found) -- extract the months & leave out their days
    where   found = filter (\(m, a@(d:ds)) -> and [length a == 31,
                d == Friday]) ms

-- take all days from 1900 until 2100, grouping them by years, then by
-- months, and calculating whether they have any 5-weekended months
-- or not

calendar :: [MonthlyCalendar]
calendar = map months $ yearsFrom1900

fiveWeekends1900To2100 :: [(Year, [Month])]
fiveWeekends1900To2100 = takeWhile (\(y, _) -> y <= 2100) $
    map whichFiveWeekends calendar

main = do
    -- count the number of years with 5 weekends
    let answer1 = foldl (\c (_, m) -> c + length m) 0 fiveWeekends1900To2100
    -- take only the years with 5-weekended months
        answer2 = filter (\(_, m) -> not $ null m) fiveWeekends1900To2100
    -- take only the years without 5-weekended months
        answer30 = filter (\(_, m) -> null m) fiveWeekends1900To2100
    -- count how many years without 5-weekended months there are
        answer31 = length answer30
    -- show the years without 5-weekended months
        answer32 = intercalate ", " $ map (\(y, m) -> show y) answer30
    putStrLn $ "There are " ++ show answer1 ++ " months with 5 weekends between 1900 and 2100."
    putStrLn "\nThe first ones are:"
    mapM_ (putStrLn . formatMonth) $ take 5 $ answer2
    putStrLn "\nThe last ones are:"
    mapM_ (putStrLn . formatMonth) $ reverse $ take 5 $ reverse answer2
    putStrLn $ "\n" ++ show answer31 ++ " years don't have at least one five-weekened month"
    putStrLn "\nThose are:"
    putStrLn answer32

formatMonth :: (Year, [Month]) -> String
formatMonth (y, m) = show y ++ ": " ++ intercalate ", " [ show x | x <- m ]
