-module( sieve_of_eratosthenes ).

-export( [primes_upto/1] ).

primes_upto( N ) ->
	Ns = lists:seq( 2, N ),
	Dict = dict:from_list( [{X, potential_prime} || X <- Ns] ),
	{Upto_sqrt_ns, _T} = lists:split( erlang:round(math:sqrt(N)), Ns ),
	{N, Prime_dict} = lists:foldl( fun find_prime/2, {N, Dict}, Upto_sqrt_ns ),
	lists:sort( dict:fetch_keys(Prime_dict) ).



find_prime( N, {Max, Dict} ) -> find_prime( dict:find(N, Dict), N, {Max, Dict} ).

find_prime( error, _N, Acc ) -> Acc;
find_prime( {ok, _Value}, N, {Max, Dict} ) -> {Max, lists:foldl( fun dict:erase/2, Dict, lists:seq(N*N, Max, N) )}.
