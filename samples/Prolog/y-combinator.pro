:- use_module(lambda).

% The Y combinator
y(P, Arg, R) :-
	Pred = P +\Nb2^F2^call(P,Nb2,F2,P),
	call(Pred, Arg, R).


test_y_combinator :-
    % code for Fibonacci function
    Fib   = \NFib^RFib^RFibr1^(NFib < 2 ->
			         RFib = NFib
			      ;
			         NFib1 is NFib - 1,
			         NFib2 is NFib - 2,
			         call(RFibr1,NFib1,RFib1,RFibr1),
			         call(RFibr1,NFib2,RFib2,RFibr1),
			         RFib is RFib1 + RFib2
			      ),

    y(Fib, 10, FR), format('Fib(~w) = ~w~n', [10, FR]),

    % code for Factorial function
    Fact =  \NFact^RFact^RFactr1^(NFact = 1 ->
			            RFact = NFact
                                 ;
			            NFact1 is NFact - 1,
			            call(RFactr1,NFact1,RFact1,RFactr1),
			            RFact is NFact * RFact1
			         ),

    y(Fact, 10, FF), format('Fact(~w) = ~w~n', [10, FF]).
