% define a category for holding the interface
% and implementation for delegator objects

:- category(delegator).

    :- public(delegate/1).
    :- public(set_delegate/1).

    :- private(delegate_/1).
    :- dynamic(delegate_/1).

    delegate(Delegate) :-
        ::delegate_(Delegate).

    set_delegate(Delegate) :-
        ::retractall(delegate_(Delegate)),
        ::assertz(delegate_(Delegate)).

:- end_category.

% define a simpler delegator object, with a
% method, operation/1, for testing delegation

:- object(a_delegator,
    imports(delegator)).

    :- public(operation/1).

    operation(String) :-
        (   ::delegate(Delegate), Delegate::current_predicate(thing/1) ->
            % a delegate is defined that understands the method thing/1
            Delegate::thing(String)
        ;   % otherwise just use the default implementation
            String = 'default implementation'
        ).

:- end_object.

% define an interface for delegate objects

:- protocol(delegate).

    :- public(thing/1).

:- end_protocol.

% define a simple delegate

:- object(a_delegate,
    implements(delegate)).

    thing('delegate implementation').

:- end_object.

% define a simple object that doesn't implement the "delegate" interface

:- object(an_object).

:- end_object.

% test the delegation solution when this file is compiled and loaded

:- initialization((
    % without a delegate:
    a_delegator::operation(String1),
    String1 == 'default implementation',
    % with a delegate that does not implement thing/1:
    a_delegator::set_delegate(an_object),
    a_delegator::operation(String2),
    String2 == 'default implementation',
    % with a delegate that implements thing/1:
    a_delegator::set_delegate(a_delegate),
    a_delegator::operation(String3),
    String3 == 'delegate implementation'
)).
