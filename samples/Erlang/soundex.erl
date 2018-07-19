-module(soundex).
-export([soundex/1]).

soundex([]) ->
    [];
soundex(Str) ->
    [Head|Tail] = string:to_upper(Str),
    [Head | isoundex(Tail, [], todigit(Head))].

isoundex([], Acc, _) ->
    case length(Acc) of
	N when N == 3 ->
	    lists:reverse(Acc);
	N when N < 3 ->
	    isoundex([], [$0 | Acc], ignore);
	N when N > 3 ->
	    isoundex([], lists:sublist(Acc, N-2, N), ignore)
    end;
isoundex([Head|Tail], Acc, Lastn) ->
    Dig = todigit(Head),
    case Dig of
	Dig when Dig /= $0, Dig /= Lastn ->
	    isoundex(Tail, [Dig | Acc], Dig);
	_ ->
	    case Head of
		$H ->
		    isoundex(Tail, Acc, Lastn);
		$W ->
		    isoundex(Tail, Acc, Lastn);
		N when N >= $A, N =< $Z ->
		    isoundex(Tail, Acc, Dig);
		_ ->
		    isoundex(Tail, Acc, Lastn)	% This clause handles non alpha characters
	    end
    end.

todigit(Chr) ->	
    Digits = "01230120022455012623010202",
    HeadOff = Chr - $A + 1,
    case HeadOff of
	N when N > 0, N < 27 ->
	    lists:nth(HeadOff, Digits);
	_ ->					% Treat non alpha characters as a vowel
	    $0
    end.
