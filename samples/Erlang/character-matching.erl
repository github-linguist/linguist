-module(character_matching).
-export([starts_with/2,ends_with/2,contains/2]).

%% Both starts_with and ends_with are mappings to 'lists:prefix/2' and
%% 'lists:suffix/2', respectively.

starts_with(S1,S2) ->
    lists:prefix(S2,S1).

ends_with(S1,S2) ->
    lists:suffix(S2,S1).

contains(S1,S2) ->
    contains(S1,S2,1,[]).

%% While S1 is at least as long as S2 we check if S2 is its prefix,
%% storing the result if it is. When S1 is shorter than S2, we return
%% the result. NB: this will work on any arbitrary list in erlang
%% since it makes no distinction between strings and lists.
contains([_|T]=S1,S2,N,Acc) when length(S2) =< length(S1) ->
    case starts_with(S1,S2) of
        true ->
            contains(T,S2,N+1,[N|Acc]);
        false ->
            contains(T,S2,N+1,Acc)
    end;
contains(_S1,_S2,_N,Acc) ->
    Acc.
