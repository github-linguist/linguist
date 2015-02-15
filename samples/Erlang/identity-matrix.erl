%% Identity Matrix in Erlang for the Rosetta Code Wiki.
%% Implemented by Arjun Sunel

-module(identity_matrix).
-export([square_matrix/2 , identity/1]).

square_matrix(Size, Elements) ->
    [[Elements(Column, Row) || Column <- lists:seq(1, Size)] || Row <- lists:seq(1, Size)].

identity(Size) ->
    square_matrix(Size, fun(Column, Row) -> case Column of Row -> 1; _ -> 0 end end).
