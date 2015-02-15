-module( word_wrap ).

-export( [paragraph/2, task/0] ).

paragraph( String, Max_line_length ) ->
	Lines = lines( string:tokens(String, " "), Max_line_length ),
	string:join( Lines, "\n" ).

task() ->
	Paragraph = "Even today, with proportional fonts and complex layouts, there are still cases where you need to wrap text at a specified column. The basic task is to wrap a paragraph of text in a simple way in your language. If there is a way to do this that is built-in, trivial, or provided in a standard library, show that. Otherwise implement the minimum length greedy algorithm from Wikipedia.",
	io:fwrite( "~s~n~n", [paragraph(Paragraph, 72)] ),
	io:fwrite( "~s~n~n", [paragraph(Paragraph, 80)] ).



lines( [Word | T], Max_line_length ) ->
	{Max_line_length, _Length, Last_line, Lines} = lists:foldl( fun lines_assemble/2, {Max_line_length, erlang:length(Word), Word, []}, T ),
	lists:reverse( [Last_line | Lines] ).

lines_assemble( Word, {Max, Line_length, Line, Acc} ) when erlang:length(Word) + Line_length > Max -> {Max, erlang:length(Word), Word, [Line | Acc]};
lines_assemble( Word, {Max, Line_length, Line, Acc} ) -> {Max, Line_length + 1 + erlang:length(Word), Line ++ " " ++ Word, Acc}.
