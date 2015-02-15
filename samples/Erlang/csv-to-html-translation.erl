-module( csv_to_html ).

-export( [table_translation/1, task/0] ).

table_translation( CSV ) ->
	[Headers | Contents] = [string:tokens(X, ",") || X <- string:tokens( CSV, "\n")],
	Table = create_html_table:html_table( [{border, "1"}, {cellpadding, "10"}], Headers, Contents ),
	create_html_table:external_format( Table ).

task() -> table_translation( csv() ).



csv() ->
"Character,Speech
The multitude,The messiah! Show us the messiah!
Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>
The multitude,Who are you?
Brians mother,I'm his mother; that's who!
The multitude,Behold his mother! Behold his mother!".
