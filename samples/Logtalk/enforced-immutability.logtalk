:- object(immutable).

    % forbid using (complementing) categories for adding to or
    % modifying (aka hot patching) the object
    :- set_logtalk_flag(complements, deny).
    % forbid dynamically adding new predicates at runtime
    :- set_logtalk_flag(dynamic_declarations, deny).

    :- public(foo/1).
    foo(1).       % static predicate by default

    :- private(bar/2)
    bar(2, 3).    % static predicate by default

:- end_object.
