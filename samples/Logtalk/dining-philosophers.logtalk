:- category(chopstick).

    % chopstick actions (picking up and putting down) are synchronized using a notification
    % such that a chopstick can only be handled by a single philosopher at a time:

    :- public(pick_up/0).
    pick_up :-
        threaded_wait(available).

    :- public(put_down/0).
    put_down :-
        threaded_notify(available).

:- end_category.


:- object(cs1,
    imports(chopstick)).

    :- threaded.
    :- initialization(threaded_notify(available)).

:- end_object.


:- object(cs2,
    imports(chopstick)).

    :- threaded.
    :- initialization(threaded_notify(available)).

:- end_object.


:- object(cs3,
    imports(chopstick)).

    :- threaded.
    :- initialization(threaded_notify(available)).

:- end_object.


:- object(cs4,
    imports(chopstick)).

    :- threaded.
    :- initialization(threaded_notify(available)).

:- end_object.


:- object(cs5,
    imports(chopstick)).

    :- threaded.
    :- initialization(threaded_notify(available)).

:- end_object.


:- category(philosopher).

    :- public(left_chopstick/1).
    :- public(right_chopstick/1).
    :- public(run/2).

    :- private(message/1).
    :- synchronized(message/1).

    :- uses(random, [random/3]).

    run(0, _) :-
        this(Philosopher),
        message([Philosopher, ' terminated.']).

    run(Count, MaxTime) :-
        Count > 0,
        think(MaxTime),
        eat(MaxTime),
        Count2 is Count - 1,
        run(Count2, MaxTime).

    think(MaxTime):-
        this(Philosopher),
        random(1, MaxTime, ThinkTime),
        message(['Philosopher ', Philosopher, ' thinking for ', ThinkTime, ' seconds.']),
        thread_sleep(ThinkTime).

    eat(MaxTime):-
        this(Philosopher),
        random(1, MaxTime, EatTime),
        ::left_chopstick(LeftStick),
        ::right_chopstick(RightStick),
        LeftStick::pick_up,
        RightStick::pick_up,
        message(['Philosopher ', Philosopher, ' eating for ', EatTime, ' seconds with chopsticks ', LeftStick, ' and ', RightStick, '.']),
        thread_sleep(EatTime),
        ::LeftStick::put_down,
        ::RightStick::put_down.

    % writing a message needs to be synchronized as it's accomplished
    % using a combination of individual write/1 and nl/0 calls:
    message([]) :-
        nl,
        flush_output.
    message([Atom| Atoms]) :-
        write(Atom),
        message(Atoms).

:- end_category.


:- object(aristotle,
    imports(philosopher)).

    left_chopstick(cs1).
    right_chopstick(cs2).

:- end_object.


:- object(kant,
    imports(philosopher)).

    left_chopstick(cs2).
    right_chopstick(cs3).

:- end_object.


:- object(spinoza,
    imports(philosopher)).

    left_chopstick(cs3).
    right_chopstick(cs4).

:- end_object.


:- object(marx,
    imports(philosopher)).

    left_chopstick(cs4).
    right_chopstick(cs5).

:- end_object.


:- object(russell,
    imports(philosopher)).

    left_chopstick(cs1).    % change order so that the chopsticks are picked
    right_chopstick(cs5).   % in different order from the other philosophers

:- end_object.
