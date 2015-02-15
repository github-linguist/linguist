-module( create_html_table ).

-export( [external_format/1, html_table/3, task/0] ).

external_format( XML ) -> remove_quoutes( lists:flatten(xmerl:export_simple_content([XML], xmerl_xml)) ).

html_table( Table_options, Headers, Contents ) ->
	Header = html_table_header( Headers ),
	Records = [html_table_record(X) || X <- Contents],
	{table, Table_options, [Header | Records]}.

task() ->
	Headers = [" ", "X", "Y", "Z"],
	Contents = [[erlang:integer_to_list(X), random(), random(), random()] || X <- lists:seq(1, 3)],
	external_format( html_table([{border, 1}, {cellpadding, 10}], Headers, Contents) ).



html_table_header( Items ) -> {tr, [], [{th, [], [X]} || X <- Items]}.

html_table_record( Items ) -> {tr, [], [{td, [], [X]} || X <- Items]}.

random() -> erlang:integer_to_list( random:uniform(1000) ).

remove_quoutes( String ) -> lists:flatten( string:tokens(String, "\"") ).
