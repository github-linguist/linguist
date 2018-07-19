% Implemented by Arjun Sunel
-module(pi_calculation).
-export([main/0]).

main() ->
	pi(1,0,1,1,3,3,0).

pi(Q,R,T,K,N,L,C)   ->
	
	if C=:=50 ->
		io:format("\n"),
		pi(Q,R,T,K,N,L,0)  ;

	true ->

		if
			(4*Q + R-T) < (N*T) ->
			io:format("~p",[N]),
	 		P = 10*(R-N*T),
	 		pi(Q*10 , P, T , K , ((10*(3*Q+R)) div T)-10*N , L,C+1);

		true ->
			P = (2*Q+R)*L,
			M = (Q*(7*K)+2+(R*L)) div (T*L),
			H  = L+2,
			J =K+ 1,
			pi(Q*K, P , T*L ,J,M,H,C)
	 	end
 	end.	
