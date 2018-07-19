1> S = "". % erlang strings are actually lists, so the empty string is the same as the empty list [].
[]
2> length(S).
0
3> case S of [] -> empty; [H|T] -> not_empty end.
empty
4> case "aoeu" of [] -> empty; [H|T] -> not_empty end.
not_empty
