-module( anonymous_recursion ).
-export( [fib/1, fib_internal/1] ).

fib( N ) when N >= 0 ->
	fib( N, 1, 0 ).

fib_internal( N ) when N >= 0 ->
	Fun = fun (_F, 0, _Next, Acc ) -> Acc;
		(F, N, Next, Acc) -> F( F, N - 1, Acc+Next, Next )
		end,
	Fun( Fun, N, 1, 0 ).


fib( 0, _Next, Acc ) -> Acc;
fib( N, Next, Acc ) -> fib( N - 1, Acc+Next, Next ).
