:- module(format_spec, [ format_error/2
                       , format_spec/2
                       , format_spec//1
                       , spec_arity/2
                       , spec_types/2
                       ]).

:- use_module(library(dcg/basics), [eos//0, integer//1, string_without//2]).
:- use_module(library(error)).
:- use_module(library(when), [when/2]).

% TODO loading this module is optional
% TODO it's for my own convenience during development
%:- use_module(library(mavis)).

%% format_error(+Goal, -Error:string) is nondet.
%
%  True if Goal exhibits an Error in its format string. The
%  Error string describes what is wrong with Goal. Iterates each
%  error on backtracking.
%
%  Goal may be one of the following predicates:
%
%    * format/2
%    * format/3
%    * debug/3
format_error(format(Format,Args), Error) :-
    format_error_(Format, Args,Error).
format_error(format(_,Format,Args), Error) :-
    format_error_(Format,Args,Error).
format_error(debug(_,Format,Args), Error) :-
    format_error_(Format,Args,Error).

format_error_(Format,Args,Error) :-
    format_spec(Format, Spec),
    !,
    is_list(Args),
    spec_types(Spec, Types),
    types_error(Args, Types, Error).
format_error_(Format,_,Error) :-
    % \+ format_spec(Format, _),
    format(string(Error), "Invalid format string: ~q", [Format]).

types_error(Args, Types, Error) :-
    length(Types, TypesLen),
    length(Args, ArgsLen),
    TypesLen =\= ArgsLen,
    !,
    format( string(Error)
          , "Wrong argument count. Expected ~d, got ~d"
          , [TypesLen, ArgsLen]
          ).
types_error(Args, Types, Error) :-
    types_error_(Args, Types, Error).

types_error_([Arg|_],[Type|_],Error) :-
    ground(Arg),
    \+ is_of_type(Type,Arg),
    message_to_string(error(type_error(Type,Arg),_Location),Error).
types_error_([_|Args],[_|Types],Error) :-
    types_error_(Args, Types, Error).


% check/0 augmentation
:- multifile check:checker/2.
:- dynamic check:checker/2.
check:checker(format_spec:checker, "format/2 strings and arguments").

:- dynamic format_fail/3.

checker :-
    prolog_walk_code([ module_class([user])
                     , infer_meta_predicates(false)
                     , autoload(false)  % format/{2,3} are always loaded
                     , undefined(ignore)
                     , trace_reference(_)
                     , on_trace(check_format)
                     ]),
    retract(format_fail(Goal,Location,Error)),
    print_message(warning, format_error(Goal,Location,Error)),
    fail.  % iterate all errors
checker.  % succeed even if no errors are found

check_format(Module:Goal, _Caller, Location) :-
    predicate_property(Module:Goal, imported_from(Source)),
    memberchk(Source, [system,prolog_debug]),
    can_check(Goal),
    format_error(Goal, Error),
    assert(format_fail(Goal, Location, Error)),
    fail.
check_format(_,_,_).  % succeed to avoid printing goals

% true if format_error/2 can check this goal
can_check(Goal) :-
    once(clause(format_error(Goal,_),_)).

prolog:message(format_error(Goal,Location,Error)) -->
    prolog:message_location(Location),
    ['~n    In goal: ~q~n    ~s'-[Goal,Error]].


%% format_spec(-Spec)//
%
%  DCG for parsing format strings. It doesn't yet generate format
%  strings from a spec.  See format_spec/2 for details.
format_spec([]) -->
    eos.
format_spec([escape(Numeric,Modifier,Action)|Rest]) -->
    "~",
    numeric_argument(Numeric),
    modifier_argument(Modifier),
    action(Action),
    format_spec(Rest).
format_spec([text(String)|Rest]) -->
    { when((ground(String);ground(Codes)),string_codes(String, Codes)) },
    string_without("~", Codes),
    { Codes \= [] },
    format_spec(Rest).


%% format_spec(+Format, -Spec:list) is semidet.
%
%  Parse a format string.  Each element of Spec is one of the following:
%
%    * `text(Text)` - text sent to the output as is
%    * `escape(Num,Colon,Action)` - a format escape
%
%  `Num` represents the optional numeric portion of an esape. `Colon`
%  represents the optional colon in an escape. `Action` is an atom
%  representing the action to be take by this escape.
format_spec(Format, Spec) :-
    when((ground(Format);ground(Codes)),text_codes(Format, Codes)),
    once(phrase(format_spec(Spec), Codes, [])).

%% spec_arity(+FormatSpec, -Arity:positive_integer) is det.
%
%  True if FormatSpec requires format/2 to have Arity arguments.
spec_arity(Spec, Arity) :-
    spec_types(Spec, Types),
    length(Types, Arity).


%% spec_types(+FormatSpec, -Types:list(type)) is det.
%
%  True if FormatSpec requires format/2 to have arguments of Types. Each
%  value of Types is a type as described by error:has_type/2. This
%  notion of types is compatible with library(mavis).
spec_types(Spec, Types) :-
    phrase(spec_types(Spec), Types).

spec_types([]) -->
    [].
spec_types([Item|Items]) -->
    item_types(Item),
    spec_types(Items).

item_types(text(_)) -->
    [].
item_types(escape(Numeric,_,Action)) -->
    numeric_types(Numeric),
    action_types(Action).

numeric_types(number(_)) -->
    [].
numeric_types(character(_)) -->
    [].
numeric_types(star) -->
    [number].
numeric_types(nothing) -->
    [].

action_types(Action) -->
    { atom_codes(Action, [Code]) },
    { action_types(Code, Types) },
    phrase(Types).


%% text_codes(Text:text, Codes:codes).
text_codes(Var, Codes) :-
    var(Var),
    !,
    string_codes(Var, Codes).
text_codes(Atom, Codes) :-
    atom(Atom),
    !,
    atom_codes(Atom, Codes).
text_codes(String, Codes) :-
    string(String),
    !,
    string_codes(String, Codes).
text_codes(Codes, Codes) :-
    is_of_type(codes, Codes).


numeric_argument(number(N)) -->
    integer(N).
numeric_argument(character(C)) -->
    "`",
    [C].
numeric_argument(star) -->
    "*".
numeric_argument(nothing) -->
    "".


modifier_argument(colon) -->
    ":".
modifier_argument(no_colon) -->
    \+ ":".


action(Action) -->
    [C],
    { is_action(C) },
    { atom_codes(Action, [C]) }.


%% is_action(+Action:integer) is semidet.
%% is_action(-Action:integer) is multi.
%
%  True if Action is a valid format/2 action character. Iterates all
%  acceptable action characters, if Action is unbound.
is_action(Action) :-
    action_types(Action, _).

%% action_types(?Action:integer, ?Types:list(type))
%
%  True if Action consumes arguments matching Types. An action (like
%  `~`), which consumes no arguments, has `Types=[]`.  For example,
%
%      ?- action_types(0'~, Types).
%      Types = [].
%      ?- action_types(0'a, Types).
%      Types = [atom].
action_types(0'~, []).
action_types(0'a, [atom]).
action_types(0'c, [integer]).  % specifically, a code
action_types(0'd, [integer]).
action_types(0'D, [integer]).
action_types(0'e, [float]).
action_types(0'E, [float]).
action_types(0'f, [float]).
action_types(0'g, [float]).
action_types(0'G, [float]).
action_types(0'i, [any]).
action_types(0'I, [integer]).
action_types(0'k, [any]).
action_types(0'n, []).
action_types(0'N, []).
action_types(0'p, [any]).
action_types(0'q, [any]).
action_types(0'r, [integer]).
action_types(0'R, [integer]).
action_types(0's, [text]).
action_types(0'@, [callable]).
action_types(0't, []).
action_types(0'|, []).
action_types(0'+, []).
action_types(0'w, [any]).
action_types(0'W, [any, list]).
