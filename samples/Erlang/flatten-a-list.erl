flatten([]) -> [];
flatten([H|T]) -> flatten(H) ++ flatten(T);
flatten(H) -> [H].
