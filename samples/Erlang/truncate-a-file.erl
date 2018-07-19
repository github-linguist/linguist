-module( truncate ).

-export( [file/2] ).

file( Name, Size ) ->
	{file_exists, true} = {file_exists, filelib:is_file( Name )},
	{ok, IO} = file:open( Name, [read, write] ),
	{ok, Max} = file:position( IO, eof ),
	{correct_size, true} = {correct_size, (Size < Max)},
	{ok, Size} = file:position( IO, {bof, Size} ),
	ok = file:truncate( IO ).
