%
-module(middle_three_digits).
-export([main/0]).

main() ->
	digits(123),
	digits(12345),
	digits(1234567),
	digits(987654321),	
	digits(10001),
	digits(-10001),
	digits(-123),	
	digits(-100),
	digits(100),
	digits(-12345),
	digits(1),
	digits(2),
	digits(-1),
	digits(-10),
	digits(2002),
	digits(-2002),
	digits(0).

digits(N) when N < 0 ->
	digits(-N);
digits(N) when (N div 100) =:= 0  ->
	io:format("too small\n");
digits(N) ->
	K=length(integer_to_list(N)),
	if (K rem 2) =:= 0 ->
		io:format("even number of digits\n");
	true ->	
		loop((K-3) div 2 , N)
	end.

loop(0, N) ->
	io:format("~3..0B~n",[N rem 1000]);	
loop(X,N)  when X>0 ->
	loop(X-1, N div 10).
