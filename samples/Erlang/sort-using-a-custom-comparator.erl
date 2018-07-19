-module( sort_using_custom_comparator ).

-export( [task/0] ).

task() ->
	lists:sort( fun longest_first_case_insensitive/2, ["this", "is", "a", "set", "of", "strings", "to", "sort", "This", "Is", "A", "Set", "Of", "Strings", "To", "Sort"] ).



longest_first_case_insensitive( String1, String2 ) when erlang:length(String1) =:= erlang:length(String2) -> string:to_lower(String1) < string:to_lower(String2);
longest_first_case_insensitive( String1, String2 ) when erlang:length(String1) =< erlang:length(String2) -> false;
longest_first_case_insensitive( _String1, _String2 ) -> true.
