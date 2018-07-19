-module(rpn).
-export([eval/1]).

parse(Expression) ->
    parse(string:tokens(Expression," "),[]).

parse([],Expression) ->
    lists:reverse(Expression);
parse(["+"|Xs],Expression) ->
    parse(Xs,[fun erlang:'+'/2|Expression]);
parse(["-"|Xs],Expression) ->
    parse(Xs,[fun erlang:'-'/2|Expression]);
parse(["*"|Xs],Expression) ->
    parse(Xs,[fun erlang:'*'/2|Expression]);
parse(["/"|Xs],Expression) ->
    parse(Xs,[fun erlang:'/'/2|Expression]);
parse(["^"|Xs],Expression) ->
    parse(Xs,[fun math:pow/2|Expression]);
parse([X|Xs],Expression) ->
    {N,_} = string:to_integer(X),
    parse(Xs,[N|Expression]).

%% The expression should be entered as a string of numbers and
%% operators separated by spaces. No error handling is included if
%% another string format is used.
eval(Expression) ->
    eval(parse(Expression),[]).

eval([],[N]) ->
    N;
eval([N|Exp],Stack) when is_number(N) ->
    NewStack = [N|Stack],
    print(NewStack),
    eval(Exp,NewStack);
eval([F|Exp],[X,Y|Stack]) ->
    NewStack = [F(Y,X)|Stack],
    print(NewStack),
    eval(Exp,NewStack).

print(Stack) ->
    lists:map(fun (X) when is_integer(X) -> io:format("~12.12b ",[X]);
                  (X) when is_float(X) -> io:format("~12f ",[X]) end, Stack),
    io:format("~n").
