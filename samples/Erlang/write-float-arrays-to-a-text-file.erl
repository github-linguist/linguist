-module( write_float_arrays ).

-export( [task/0, to_a_text_file/3, to_a_text_file/4] ).

task() ->
	File = "afile",
	Xs = [1.0, 2.0, 3.0, 1.0e11],
	Ys = [1.0, 1.4142135623730951, 1.7320508075688772, 316227.76601683791],
	Options = [{xprecision, 3}, {yprecision, 5}],
	to_a_text_file( File, Xs, Ys, Options ),
	{ok, Contents} = file:read_file( File ),
	io:fwrite( "File contents: ~p~n", [Contents] ).

to_a_text_file( File, Xs, Ys ) -> to_a_text_file( File, Xs, Ys, [] ).

to_a_text_file( File, Xs, Ys, Options ) ->
	Xprecision = proplists:get_value( xprecision, Options, 2 ),
	Yprecision = proplists:get_value( yprecision, Options, 2 ),
	Format = lists:flatten( io_lib:format("~~.~pg ~~.~pg~n", [Xprecision, Yprecision]) ),
	{ok, IO} = file:open( File, [write] ),
	[ok = io:fwrite( IO, Format, [X, Y]) || {X, Y} <- lists:zip( Xs, Ys)],
	file:close( IO ).
