%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% definitions of queue
empty(U-V) :-
    unify_with_occurs_check(U, V).

push(Queue, Value, NewQueue) :-
    append_dl(Queue, [Value|X]-X, NewQueue).


pop([X|V]-U, X, V-U) :-
    \+empty([X|V]-U).



append_dl(X-Y, Y-Z, X-Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% use of queue
queue :-
    % create an empty queue
    empty(Q),
    format('Create queue ~w~n~n', [Q]),

    % add numbers 1 and 2
    write('Add numbers 1 and 2 : '),
    push(Q, 1, Q1),
    push(Q1, 2, Q2),

    % display queue
    format('~w~n~n', [Q2]),

    % pop element
    pop(Q2, V, Q3),

    % display results
    format('Pop : Value ~w Queue : ~w~n~n', [V, Q3]),

    % test the queue
    write('Test of the queue : '),
    (   empty(Q3) -> writeln('Queue empy'); writeln('Queue not empty')), nl,

    % pop the elements
    write('Pop the queue : '),
    pop(Q3, V1, Q4),
    format('Value ~w Queue : ~w~n~n', [V1, Q4]),

    write('Pop the queue : '),
    pop(Q4, _V, _Q5).
