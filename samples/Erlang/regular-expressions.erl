match() ->
	String = "This is a string",
	case re:run(String, "string$") of
		{match,_} -> io:format("Ends with 'string'~n");
		_ -> ok
	end.

substitute() ->
	String = "This is a string",
	NewString = re:replace(String, " a ", " another ", [{return, list}]),
	io:format("~s~n",[NewString]).
