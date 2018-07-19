-module( bitcoin_address ).

-export( [task/0, validate/1] ).

task() ->
	io:fwrite( "Validate ~p~n", ["1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"] ),
	io:fwrite( "~p~n", [validate("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i")] ),
	io:fwrite( "Validate ~p~n", ["1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW622"] ),
	io:fwrite( "~p~n", [validate("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW622")] ).

validate( String ) ->
	{length_25, <<Address:21/binary, Checksum:4/binary>>} = {length_25, base58:base58_to_binary( String )},
	<<Version:1/binary, _/binary>> = Address,
	{version_0, <<0>>} = {version_0, Version},
	<<Four_bytes:4/binary, _T/binary>> = crypto:hash( sha256, crypto:hash(sha256, Address) ),
	{checksum, Checksum} = {checksum, Four_bytes},
	ok.
