empty(U-V) :-
    unify_with_occurs_check(U, V).

push(Queue, Value, NewQueue) :-
    append_dl(Queue, [Value|X]-X, NewQueue).

% when queue is empty pop fails.
pop([X|V]-U, X, V-U) :-
    \+empty([X|V]-U).

append_dl(X-Y, Y-Z, X-Z).
