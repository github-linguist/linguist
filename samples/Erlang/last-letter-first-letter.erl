-module( last_letter_first_letter ).

-export( [solve/1, task/0] ).

solve( Names ) ->
    Dict = lists:foldl( fun dict_append/2, dict:new(), Names ),
    Chains = construct_chains_in_parallel( Dict ),
%    Chains = [construct_chain_from_key(Dict, X) || X <- dict:fetch_keys(Dict)],
    lists:foldl( fun construct_chain_longest/2, [], Chains ).

task() -> solve( binary:split(names(), <<" ">>, [global]) ).



construct_chains_in_parallel( Dict ) ->
    My_pid = erlang:self(),
    Pids = [erlang:spawn( fun() -> My_pid ! {erlang:self(), construct_chain_from_key(Dict, X)} end) || X <- dict:fetch_keys(Dict)],
    [receive {X, Chain} -> Chain end || X <- Pids].

construct_chain_from_key( Dict, First_letter ) ->
	Names = construct_chain_names( dict:find(First_letter, Dict) ),
	construct_chain_from_names( Names, Dict, [] ).

construct_chain_from_names( [], _Dict, Best_chain ) -> Best_chain;
construct_chain_from_names( [{Name, Last_letter} | T], Dict, Best_chain ) ->
        New_dict = dict_delete( Name, Dict ),
        New_chain = [Name | construct_chain_from_key( New_dict, Last_letter )],
	construct_chain_from_names( T, Dict, construct_chain_longest(Best_chain, New_chain) ).

construct_chain_longest( Chain1, Chain2 ) when length(Chain1) > length(Chain2) -> Chain1;
construct_chain_longest( _Chain1, Chain2 ) -> Chain2.

construct_chain_names( {ok, {Name, Last_letter}} ) -> [{Name, Last_letter}];
construct_chain_names(	{ok, Values} ) -> Values;
construct_chain_names( error ) -> [].

dict_append( Name, Acc ) ->
	{First_letter, {Name, Last_letter}} = dict_item( Name ),
	dict:append( First_letter, {Name, Last_letter}, Acc ).

dict_item( <<First_letter, _T/binary>>=Name ) ->
	Until_last_letter = erlang:byte_size( Name ) - 1,
	<<_H:Until_last_letter/binary, Last_letter>> = Name,
	{First_letter, {Name, Last_letter}}.

dict_delete( <<First_letter, _T/binary>>=Name, Dict ) ->
	Name_last_letters = dict:fetch(First_letter, Dict),
	dict:store( First_letter, lists:keydelete(Name, 1, Name_last_letters), Dict ).

names() -> <<"audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon cresselia croagunk darmanitan deino emboar emolga exeggcute gabite girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2 porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask">>.
