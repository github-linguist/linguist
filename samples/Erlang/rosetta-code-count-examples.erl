-module(rosseta_examples).
-include_lib("xmerl/include/xmerl.hrl").

-export([main/0]).

main() ->
   application:start(inets),
   Titles = read_titles(empty),
   Result = lists:foldl(fun(Title,Acc) -> Acc + calculate_one(Title) end, 0, Titles),
   io:format("Total: ~p examples.\n",[Result]),
   application:stop(inets).

read_titles(CurrentContinue) ->
   URL0 = "http://rosettacode.org/mw/api.php?" ++
         "action=query&list=categorymembers&cmtitle=Category:Programming_Tasks" ++
         "&cmlimit=500&format=xml",
   URL =
      case CurrentContinue of
         empty -> URL0;
         _ -> URL0 ++ "&cmcontinue=" ++ CurrentContinue
      end,
   {ok,Answer} = httpc:request(URL),
   {Document,_} = xmerl_scan:string(lists:last(tuple_to_list(Answer))),
   Continue =
      [Value || #xmlAttribute{value = Value} <- xmerl_xpath:string("//@cmcontinue", Document)],
   Titles =
     [Value || #xmlAttribute{value = Value} <- xmerl_xpath:string("//@title", Document)],
   case Continue of
      []->
         Titles;
      [ContValue | _] ->
         Titles ++ read_titles(ContValue)
   end.

calculate_one(Title0) ->
   Title = replace_chars(Title0),
   URL = "http://www.rosettacode.org/w/index.php?title=" ++
         Title ++ "&action=raw",
   case httpc:request(URL) of
      {ok,Result} ->
            {match,Found} =
               re:run(lists:last(tuple_to_list(Result)), "\n=={{header(|)", [global]),
            io:format("~ts: ~p examples.\n",[Title0,length(Found)]),
            length(Found);
      {error,socket_closed_remotely} ->
         io:format("Socket closed remotely. Retry.\n"),
         calculate_one(Title0)
   end.

replace_chars(String) ->
   replace_chars(String,[]).

replace_chars([$ | T],Acc) ->
   replace_chars(T, [$_| Acc]);
replace_chars([$+| T],Acc) ->
   replace_chars(T, lists:reverse("%2B") ++ Acc);
replace_chars([8211| T],Acc) ->
   replace_chars(T, lists:reverse("%E2%80%93") ++ Acc);
replace_chars([Other| T],Acc) ->
   replace_chars(T, [Other| Acc]);
replace_chars([],Acc) ->
   lists:reverse(Acc).
