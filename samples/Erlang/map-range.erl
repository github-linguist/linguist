-module(map_range).
-export([map_value/3]).

map_value({A1,A2},{B1,B2},S) ->
    B1 + (S - A1) * (B2 - B1) / (A2 - A1).
