-module( verify_distribution_uniformity ).

-export( [naive/3] ).

naive( Generator, Times, Delta_percent ) ->
    Dict = lists:foldl( fun update_counter/2, dict:new(), lists:duplicate(Times, Generator) ),
    Values = [dict:fetch(X, Dict) || X <- dict:fetch_keys(Dict)],
    Average = lists:sum( Values ) / dict:size( Dict ),
    Delta = Average * (Delta_percent / 100),
    Fun = fun(_Key, Value) -> erlang:abs(Value - Average) > Delta end,
    Too_large_dict = dict:filter( Fun, Dict ),
    return( dict:size(Too_large_dict), Too_large_dict, Average, Delta_percent ).



return( 0, _Too_large_dict, _Average, _Delta ) -> ok;
return( _N, Too_large_dict, Average, Delta ) ->
	{error, {dict:to_list(Too_large_dict), failed_expected_average, Average, 'with_delta_%', Delta}}.

update_counter( Fun, Dict ) -> dict:update_counter( Fun(), 1, Dict ).
