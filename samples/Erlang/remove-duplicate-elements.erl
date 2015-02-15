List = [1, 2, 3, 2, 2, 4, 5, 5, 4, 6, 6, 5].
UniqueList = gb_sets:to_list(gb_sets:from_list(List)).
% Alternatively the builtin:
Unique_list = lists:usort( List ).
