-module( runtime_evaluation ).

-export( [evaluate_form/2, form_from_string/1, task/0] ).

evaluate_form( Form, {Variable_name, Value} ) ->
	Bindings = erl_eval:add_binding( Variable_name, Value, erl_eval:new_bindings() ),
	{value, Evaluation, _} = erl_eval:expr( Form, Bindings ),
	Evaluation.

form_from_string( String ) ->
	{ok, Tokens, _} = erl_scan:string( String ),
	{ok, [Form]} = erl_parse:parse_exprs( Tokens ),
	Form.

task() ->
	Form = form_from_string( "X." ),
	Variable1 = evaluate_form( Form, {'X', 1} ),
	io:fwrite( "~p~n", [Variable1] ),
	Variable2 = evaluate_form( Form, {'X', 2} ),
	io:fwrite( "~p~n", [Variable2] ),
	io:fwrite( "~p~n", [Variable2 - Variable1] ).
