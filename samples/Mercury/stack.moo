:- module sstack.

:- interface.

% We're going to call the type sstack (simple stack) because we don't want to get it
% accidentally confused with the official stack module in the standard library.
:- type sstack(T).

:- func sstack.new = sstack(T).
:- pred sstack.is_empty(sstack(T)::in) is semidet.
:- func sstack.push(sstack(T), T) = sstack(T).
:- pred sstack.pop(T::out, sstack(T)::in, sstack(T)::out) is semidet.

:- implementation.

:- import_module list.

:- type sstack(T)
   --->  sstack(list(T)).

sstack.new = sstack([]).

sstack.is_empty(sstack([])).

sstack.push(Stack0, Elem) = Stack1 :-
   Stack0 = sstack(Elems),
   Stack1 = sstack([Elem | Elems]).

sstack.pop(Elem, !Stack) :-
   !.Stack = sstack([Elem | Elems]),
   !:Stack = sstack(Elems).

:- end_module sstack.
