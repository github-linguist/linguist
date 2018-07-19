-module( dynamic_variable_names ).

-export( [task/0] ).

task() ->
    {ok,[Variable_name]} = io:fread( "Variable name? ",	"~a" ),
    Form = runtime_evaluation:form_from_string( erlang:atom_to_list(Variable_name) ++ "." ),
    io:fwrite( "~p has value ~p~n", [Variable_name, runtime_evaluation:evaluate_form(Form, {Variable_name, 42})] ).
