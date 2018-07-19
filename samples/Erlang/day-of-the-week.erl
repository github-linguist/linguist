% Implemented by bengt kleberg
-module(yuletide).
-export([main/0, sunday_years/2]).

main() ->
	[io:fwrite("25 December ~p is Sunday~n", [X]) || X <- sunday_years(2008, 2121)].

sunday_years( Start, Stop ) ->
	[X || X <- lists:seq(Start, Stop), is_sunday(calendar:day_of_the_week({X, 12, 25}))].

is_sunday( 7 ) -> true;
is_sunday( _ ) -> false.
