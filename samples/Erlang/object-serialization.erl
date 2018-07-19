-module( object_serialization ).

-export( [task/0] ).

-record( entity, {name, date} ).
-record( person, {entity, email} ).

task() ->
	Person = #person{entity=#entity{name="Cletus", date=20080808}, email="test+1@localhost.localdomain"},
	print( Person ),
	Entity = #entity{name="Entity", date=20111111},
	print( Entity ),
	ok = file:write_file( "objects.dat", erlang:term_to_binary([Person, Entity]) ),
	{ok, Binary} =  file:read_file( "objects.dat" ),
	[New_person, New_entity] = erlang:binary_to_term( Binary ),
	io:fwrite( "Deserialized\n" ),
	print( New_person ),
	print( New_entity ).
	


print( #entity{name=Name, date=Date} ) ->
	io:fwrite( "Entity: " ),
	io:fwrite( "name: ~p, date: ~p~n", [Name, Date] );
print( #person{entity=Entity, email=Email} ) ->
	io:fwrite( "Person: " ),
	print( Entity ),
	io:fwrite( "\temail: ~p~n", [Email] ).
