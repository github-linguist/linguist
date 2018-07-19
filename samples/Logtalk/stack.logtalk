:- object(stack).

    :- public(push/3).
    push(Element, Stack, [Element| Stack]).

    :- public(pop/3).
    pop([Top| Stack], Top, Stack).

    :- public(empty/1)
    empty([]).

:- end_object.
