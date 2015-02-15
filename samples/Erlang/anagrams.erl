-module(anagrams).
-compile(export_all).

play() ->
    {ok, P} = file:read_file('unixdict.txt'),
    D = dict:new(),
    E=fetch(string:tokens(binary_to_list(P), "\n"), D),
    get_value(dict:fetch_keys(E), E).

fetch([H|T], D) ->
    fetch(T, dict:append(lists:sort(H), H, D));
fetch([], D) ->
    D.

get_value(L, D) -> get_value(L,D,1,[]).
get_value([H|T], D, N, L) ->
    Var = dict:fetch(H,D),
    Len = length(Var),
    if
        Len > N ->
            get_value(T, D, Len, [Var]);
        Len == N ->
            get_value(T, D, Len, [Var | L]);
        Len < N ->
            get_value(T, D, N, L)
    end;

get_value([], _, _, L) ->
    L.
