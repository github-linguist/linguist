-module(scraping).
-export([main/0]).
-define(Url, "http://tycho.usno.navy.mil/cgi-bin/timer.pl").
-define(Match, "<BR>(.+ UTC)").

main() ->
	inets:start(),
	{ok, {_Status, _Header, HTML}} = httpc:request(?Url),
	{match, [Time]} = re:run(HTML, ?Match, [{capture, all_but_first, binary}]),
	io:format("~s~n",[Time]).
