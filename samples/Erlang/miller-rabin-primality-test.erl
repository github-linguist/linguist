-module(miller_rabin).

-export([is_prime/1, power/2]).

is_prime(1) -> false;
is_prime(2) -> true;
is_prime(3) -> true;
is_prime(N) when N > 3, ((N rem 2) == 0) -> false;
is_prime(N) when ((N rem 2) ==1), N < 341550071728321 ->
 			is_mr_prime(N, proving_bases(N));
is_prime(N) when ((N rem 2) == 1) ->
			is_mr_prime(N, random_bases(N, 100)).


proving_bases(N) when N < 1373653 ->
	[2, 3];
proving_bases(N) when N < 9080191 ->
    [31, 73];
proving_bases(N) when N < 25326001 ->
	[2, 3, 5];
proving_bases(N) when N < 3215031751 ->
	[2, 3, 5, 7];
proving_bases(N) when N < 4759123141 ->
    [2, 7, 61];
proving_bases(N) when N < 1122004669633 ->
	[2, 13, 23, 1662803];
proving_bases(N) when N < 2152302898747 ->
	[2, 3, 5, 7, 11];
proving_bases(N) when N < 3474749660383 ->
    [2, 3, 5, 7, 11, 13];
proving_bases(N) when N < 341550071728321 ->
    [2, 3, 5, 7, 11, 13, 17].


is_mr_prime(N, As) when N>2, N rem 2 == 1 ->
    {D, S} = find_ds(N),
         %% this is a test for compositeness; the two case patterns disprove
         %%    compositeness.
    not lists:any(fun(A) ->
                          case mr_series(N, A, D, S) of
                              [1|_] -> false;                     % first elem of list = 1
                              L     -> not lists:member(N-1, L)   % some elem of list = N-1
                          end
                  end,
                  As).


find_ds(N) ->
    find_ds(N-1, 0).


find_ds(D, S) ->
    case D rem 2 == 0 of
        true ->
            find_ds(D div 2, S+1);
        false ->
            {D, S}
    end.


mr_series(N, A, D, S) when N rem 2 == 1 ->
    Js = lists:seq(0, S),
    lists:map(fun(J) -> pow_mod(A, power(2, J)*D, N) end, Js).


pow_mod(B, E, M) ->
    case E of
        0 -> 1;
        _ -> case ((E rem 2) == 0) of
                 true  -> (power(pow_mod(B, (E div 2), M), 2)) rem M;
                 false -> (B*pow_mod(B, E-1, M)) rem M
             end
    end.


random_bases(N, K) ->
    [basis(N) || _ <- lists:seq(1, K)].


basis(N) when N>2 ->
    1 + random:uniform(N-3).  % random:uniform returns a single random number in range 1 -> N-3, to which is added 1, shifting the range to 2 -> N-2


power(B, E) ->
    power(B, E, 1).

power(_, 0, Acc) ->
    Acc;
power(B, E, Acc) ->
    power(B, E - 1, B * Acc).
