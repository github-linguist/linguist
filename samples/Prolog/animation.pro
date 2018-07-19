:- use_module(library(pce)).

animation :-
    new(D, window('Animation')),
    new(Label, label(hello, 'Hello world ! ')),
    send(D, display, Label, point(1,10)),
    new(@animation, animation(Label)),
    send(D, recogniser,
         new(_G, my_click_gesture(left, ''))),

    send(D, done_message, and(message(@animation, free),
                  message(@receiver, destroy))),
    send(D, open),
    send(@animation?mytimer, start).


:- pce_begin_class(animation(label), object).
variable(label, object,  both, "Display window").
variable(delta,    object, both,  "increment of the angle").
variable(mytimer, timer, both, "timer of the animation").

initialise(P, W:object) :->
        "Creation of the object"::
        send(P, label, W),
        send(P, delta, to_left),
    send(P, mytimer, new(_, timer(0.5,message(P, anim_message)))).

% method called when the object is destroyed
% first the timer is stopped
% then all the resources are freed
unlink(P) :->
    send(P?mytimer, stop),
    send(P, send_super, unlink).


% message processed by the timer
anim_message(P) :->
    get(P, label, L),
    get(L, selection, S),
    get(P, delta, Delta),
    compute(Delta, S, S1),
    new(A, name(S1)),
    send(L, selection, A).


:- pce_end_class.

:- pce_begin_class(my_click_gesture, click_gesture,
           "Click in a window").

class_variable(button, button_name, left,
           "By default click with left button").

terminate(G, Ev:event) :->
    send(G, send_super, terminate, Ev),
    get(@animation, delta, D),
    (   D = to_left -> D1 = to_right; D1 = to_left),
    send(@animation, delta, D1).

:- pce_end_class.


% compute next text to be dispalyed
compute(to_right, S, S1) :-
    get(S, size, Len),
    Len1 is Len - 1,
    get(S, sub, Len1, Str),
    get(S, delete_suffix, Str, V),
    get(Str, append, V, S1).

compute(to_left, S, S1) :-
    get(S, sub, 0, 1, Str),
    get(S, delete_prefix, Str, V),
    get(V, append, Str, S1).
