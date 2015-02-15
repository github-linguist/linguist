-module(pangram).
-export([is_pangram/1]).

is_pangram(String) ->
  ordsets:is_subset(lists:seq($a, $z), ordsets:from_list(string:to_lower(String))).
