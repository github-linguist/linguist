1> {ok, Tokens, _} = erl_scan:string("X + 4 * lists:sum([1,2,3,4]).").
...
2> {ok, [Form]} = erl_parse:parse_exprs(Tokens).
...
3> Bindings = erl_eval:add_binding('X', 17, erl_eval:new_bindings()).
[{'X',17}]
4> {value, Value, _} = erl_eval:expr(Form, Bindings).
{value,57,[{'X',17}]}
5> Value.
57
