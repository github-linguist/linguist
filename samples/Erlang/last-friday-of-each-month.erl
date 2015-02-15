-module( last_date_each_month ).

-export( [monday/1, tuesday/1, wednesday/1, thursday/1, friday/1, saturday/1, sunday/1] ).

monday( Year ) -> last( Year, 1 ).
tuesday( Year ) -> last( Year, 2 ).
wednesday( Year ) -> last( Year, 3 ).
thursday( Year ) -> last( Year, 4 ).
friday( Year ) -> last( Year, 5 ).
saturday( Year ) -> last( Year, 6 ).
sunday( Year ) -> last( Year, 7 ).



last( Year, Week_day ) ->
    Months = lists:seq( 1, 12 ),
    Months_days = [{X, Y} || X <- Months, Y <- lists:seq(calendar:last_day_of_the_month(Year, X), calendar:last_day_of_the_month(Year, X) - 7, -1), calendar:valid_date(Year, X, Y), calendar:day_of_the_week(Year, X, Y) =:= Week_day],
    [{Year, X, proplists:get_value(X, Months_days)} || X <- Months].
