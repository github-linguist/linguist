-module( table_creation ).

-export( [task/0] ).

-record( address, {id, street, city, zip} ).

task() ->
	mnesia:start(),
	mnesia:create_table( address, [{attributes, record_info(fields, address)}] ).
