is_numeric(L) ->
    Float = (catch erlang:list_to_float(L)),
    Int = (catch erlang:list_to_integer(L)),
    is_number(Float) orelse is_number(Int).
