:- object(concurrency).

    :- initialization(output).

    output :-
        threaded((
            write('Enjoy'),
            write('Rosetta'),
            write('Code')
        )).

:- end_object.
