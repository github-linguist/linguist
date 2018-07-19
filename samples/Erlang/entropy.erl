-module( entropy ).

-export( [shannon/1, task/0] ).

shannon( String ) -> shannon_information_content( lists:foldl(fun count/2, dict:new(), String), erlang:length(String) ).

task() -> shannon( "1223334444" ).



count( Character, Dict ) -> dict:update_counter( Character, 1, Dict ).

shannon_information_content( Dict, String_length ) ->
	{_String_length, Acc} = dict:fold( fun shannon_information_content/3, {String_length, 0.0}, Dict ),
	Acc / math:log( 2 ).

shannon_information_content( _Character, How_many, {String_length, Acc} ) ->
        Frequency = How_many / String_length,
	{String_length, Acc - (Frequency * math:log(Frequency))}.
