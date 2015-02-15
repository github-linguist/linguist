#!/usr/bin/env escript
main([]) -> main(["unixdict.txt"]);

main([DictFile]) ->
  Dict = sets:from_list(read_lines(DictFile)),
  Semordnilaps =
    lists:filter(fun([W,R]) -> W < R end,
      lists:map(fun(W) -> [W, lists:reverse(W)] end,
        semordnilaps(Dict))),
  io:fwrite("There are ~b semordnilaps in ~s~n",
            [length(Semordnilaps), DictFile]),
  lists:map(fun([W,R]) -> io:fwrite("~s/~s~n", [W, R]) end,
            lists:sort(lists:sublist(shuffle(Semordnilaps),1,5))).

read_lines(Filename) when is_list(Filename) ->
  { ok, File } = file:open(Filename, [read]),
  read_lines(File);

read_lines(File) when is_pid(File) ->
   case file:read_line(File) of
    {ok, Data} -> [chop(Data) | read_lines(File)];
    eof        -> []
   end.

is_semordnilap(Word, Dict) ->
  Rev = lists:reverse(Word),
  sets:is_element(Word, Dict) and sets:is_element(Rev, Dict).

semordnilaps(Dict) ->
  lists:filter(fun(W) -> is_semordnilap(W, Dict) end, sets:to_list(Dict)).

shuffle(List) ->
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])].

chop(L) -> [_|T] = lists:reverse(L), lists:reverse(T).
