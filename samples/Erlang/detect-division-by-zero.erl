div_check(X,Y) ->
    case catch X/Y of
        {'EXIT',_} -> true;
        _ -> false
    end.
