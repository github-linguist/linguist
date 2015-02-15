-module( anagrams_deranged ).
-export( [task/0, words_from_url/1] ).

task() ->
       find_unimplemented_tasks:init_http(),
       Words = words_from_url( "http://www.puzzlers.org/pub/wordlists/unixdict.txt" ),
       Anagram_dict = anagrams:fetch( Words, dict:new() ),
       Deranged_anagrams = deranged_anagrams( Anagram_dict ),
       {_Length, Longest_anagrams} = dict:fold( fun keep_longest/3, {0, []}, Deranged_anagrams ),
       Longest_anagrams.

words_from_url( URL ) ->
	{ok, {{_HTTP, 200, "OK"}, _Headers, Body}} = httpc:request( URL ),
	string:tokens( Body, "\n" ).



deranged_anagrams( Dict ) ->
        Deranged_dict = dict:map( fun deranged_words/2, Dict ),
        dict:filter( fun is_anagram/2, Deranged_dict ).

deranged_words( _Key, [H | T] ) ->
        [{H, X} || X <- T, is_deranged_word(H, X)].

keep_longest( _Key, [{One, _} | _]=New, {Length, Acc} ) ->
        keep_longest_new( erlang:length(One), Length, New, Acc ).

keep_longest_new( New_length, Acc_length, New, _Acc ) when New_length > Acc_length ->
        {New_length, New};
keep_longest_new( New_length, Acc_length, New, Acc ) when New_length =:= Acc_length ->
        {Acc_length, Acc ++ New};
keep_longest_new( _New_length, Acc_length, _New, Acc ) ->
        {Acc_length, Acc}.

is_anagram( _Key, [] ) -> false;
is_anagram( _Key, _Value ) -> true.

is_deranged_word( Word1, Word2 ) ->
        lists:all( fun is_deranged_char/1, lists:zip(Word1, Word2) ).

is_deranged_char( {One, Two} ) -> One =/= Two.
