-module( xml_output ).

-export( [task/0] ).

-include_lib("xmerl/include/xmerl.hrl").

task() ->
    Data = {'CharacterRemarks', [], [{'Character', [{name, X}], [[Y]]} || {X, Y} <- contents()]},
    lists:flatten( xmerl:export_simple([Data], xmerl_xml) ).


contents() -> [{"April", "Bubbly: I'm > Tam and <= Emily"}, {"Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\""}, {"Emily", "Short & shrift"}].
