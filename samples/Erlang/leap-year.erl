-module(gregorian).
-export([leap/1]).

leap( Year ) -> calendar:is_leap_year( Year ).
