-module( ordered_words ).

-export( [is_ordered/1, task/0] ).

is_ordered( Word ) -> lists:sort( Word ) =:=  Word.

task() ->
    ok = find_unimplemented_tasks:init_http(),
    Ordered_words = [X || X <- words(), is_ordered(X)],
    Sorted_longest_length_first = lists:reverse( sort_with_length( Ordered_words ) ),
    [{Max_length, _Word1} | _T] = Sorted_longest_length_first,
    Longest_length_first = lists:takewhile( fun({Length, _Word2}) -> Length =:= Max_length end, Sorted_longest_length_first ),
    [X || {_Length, X} <- Longest_length_first].



sort_with_length( Words ) ->
    Words_with_length_first = [{erlang:length(X), X} || X <- Words],
    lists:sort( Words_with_length_first ).

words() -> anagrams_deranged:words_from_url( "http://www.puzzlers.org/pub/wordlists/unixdict.txt" ).
