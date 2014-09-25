:- module(func, [ op(675, xfy, ($))
                , op(650, xfy, (of))
                , ($)/2
                , (of)/2
                ]).
:- use_module(library(list_util), [xfy_list/3]).
:- use_module(library(function_expansion)).
:- use_module(library(arithmetic)).
:- use_module(library(error)).


% true if the module whose terms are being read has specifically
% imported library(func).
wants_func :-
    prolog_load_context(module, Module),
    Module \== func,  % we don't want func sugar ourselves
    predicate_property(Module:of(_,_),imported_from(func)).


%%  compile_function(+Term, -In, -Out, -Goal) is semidet.
%
%   True if Term represents a function from In to Out
%   implemented by calling Goal.  This multifile hook is
%   called by $/2 and of/2 to convert a term into a goal.
%   It's used at compile time for macro expansion.
%   It's used at run time to handle functions which aren't
%   known at compile time.
%   When called as a hook, Term is guaranteed to be =nonvar=.
%
%   For example, to treat library(assoc) terms as functions which
%   map a key to a value, one might define:
%
%       :- multifile compile_function/4.
%       compile_function(Assoc, Key, Value, Goal) :-
%           is_assoc(Assoc),
%           Goal = get_assoc(Key, Assoc, Value).
%
%   Then one could write:
%
%       list_to_assoc([a-1, b-2, c-3], Assoc),
%       Two = Assoc $ b,
:- multifile compile_function/4.
compile_function(Var, _, _, _) :-
    % variables storing functions must be evaluated at run time
    % and can't be compiled, a priori, into a goal
    var(Var),
    !,
    fail.
compile_function(Expr, In, Out, Out is Expr) :-
    % arithmetic expression of one variable are simply evaluated
    \+ string(Expr),  % evaluable/1 throws exception with strings
    arithmetic:evaluable(Expr),
    term_variables(Expr, [In]).
compile_function(F, In, Out, func:Goal) :-
    % composed functions
    function_composition_term(F),
    user:function_expansion(F, func:Functor, true),
    Goal =.. [Functor,In,Out].
compile_function(F, In, Out, Goal) :-
    % string interpolation via format templates
    format_template(F),
    ( atom(F) ->
        Goal = format(atom(Out), F, In)
    ; string(F) ->
        Goal = format(string(Out), F, In)
    ; error:has_type(codes, F) ->
        Goal = format(codes(Out), F, In)
    ; fail  % to be explicit
    ).
compile_function(Dict, In, Out, Goal) :-
    is_dict(Dict),
    Goal = get_dict(In, Dict, Out).

%%	$(+Function, +Argument) is det.
%
%	Apply Function to an Argument.  A Function is any predicate
%	whose final argument generates output and whose penultimate argument
%	accepts input.
%
%	This is realized by expanding function application to chained
%	predicate calls at compile time.  Function application itself can
%	be chained.
%
%	==
%	Reversed = reverse $ sort $ [c,d,b].
%	==
:- meta_predicate $(2,+).
$(_,_) :-
    throw(error(permission_error(call, predicate, ($)/2),
          context(_, '$/2 must be subject to goal expansion'))).

user:function_expansion($(F,X), Y, Goal) :-
    wants_func,
    ( func:compile_function(F, X, Y, Goal) ->
        true
    ; var(F) -> Goal =      % defer until run time
        ( func:compile_function(F, X, Y, P) ->
            call(P)
        ; call(F, X, Y)
        )
    ; Goal = call(F, X, Y)
    ).


%%	of(+F, +G) is det.
%
%	Creates a new function by composing F and G.  The functions are
%	composed at compile time to create a new, compiled predicate which
%	behaves like a function.  Function composition can be chained.
%	Composed functions can also be applied with $/2.
%
%	==
%	Reversed = reverse of sort $ [c,d,b].
%	==
:- meta_predicate of(2,2).
of(_,_).


%%  format_template(Format) is semidet.
%
%   True if Format is a template string suitable for format/3.
%   The current check is very naive and should be improved.
format_template(Format) :-
    atom(Format), !,
    atom_codes(Format, Codes),
    format_template(Codes).
format_template(Format) :-
    string(Format),
    !,
    string_codes(Format, Codes),
    format_template(Codes).
format_template(Format) :-
    error:has_type(codes, Format),
    memberchk(0'~, Format).  % ' fix syntax highlighting


% True if the argument is a function composition term
function_composition_term(of(_,_)).

% Converts a function composition term into a list of functions to compose
functions_to_compose(Term, Funcs) :-
    functor(Term, Op, 2),
    Op = (of),
    xfy_list(Op, Term, Funcs).

% Thread a state variable through a list of functions.  This is similar
% to a DCG expansion, but much simpler.
thread_state([], [], Out, Out).
thread_state([F|Funcs], [Goal|Goals], In, Out) :-
    ( compile_function(F, In, Tmp, Goal) ->
        true
    ; var(F) ->
        instantiation_error(F)
    ; F =.. [Functor|Args],
      append(Args, [In, Tmp], NewArgs),
      Goal =.. [Functor|NewArgs]
    ),
    thread_state(Funcs, Goals, Tmp, Out).

user:function_expansion(Term, func:Functor, true) :-
    wants_func,
    functions_to_compose(Term, Funcs),
    debug(func, 'building composed function for: ~w', [Term]),
    variant_sha1(Funcs, Sha),
    format(atom(Functor), 'composed_function_~w', [Sha]),
    debug(func, '  name: ~s', [Functor]),
    ( func:current_predicate(Functor/2) ->
        debug(func, '  composed predicate already exists', [])
    ; true ->
        reverse(Funcs, RevFuncs),
        thread_state(RevFuncs, Threaded, In, Out),
        xfy_list(',', Body, Threaded),
        Head =.. [Functor, In, Out],
        func:assert(Head :- Body),
        func:compile_predicates([Functor/2])
    ).


% support foo(x,~,y) evaluation
user:function_expansion(Term, Output, Goal) :-
    wants_func,
    compound(Term),

    % has a single ~ argument
    setof( X
         , ( arg(X,Term,Arg), Arg == '~' )
         , [N]
         ),

    % replace ~ with a variable
    Term =.. [Name|Args0],
    nth1(N, Args0, ~, Rest),
    nth1(N, Args, Output, Rest),
    Goal =.. [Name|Args].
