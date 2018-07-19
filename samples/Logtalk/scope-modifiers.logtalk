:- public(foo/1).     % predicate can be called from anywhere

:- protected(bar/2).  % predicate can be called from the declaring entity and its descendants

:- private(baz/3).    % predicate can only be called from the declaring entity

:- object(object,     % predicates declared in the protocol become private for the object
    implements(private::protocol)).

:- category(object,   % predicates declared in the protocol become protected for the category
    implements(protected::protocol)).

:- protocol(extended, % no change to the scope of the predicates inherited from the extended protocol
    extends(public::minimal)).
