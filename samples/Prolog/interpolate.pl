:- module(interpolate, ['$interpolate_macro_sentinel'/0]).
:- use_module(library(dcg/basics), [prolog_var_name//1, string//1]).
:- use_module(library(function_expansion)).
:- use_module(library(error), [type_error/2]).

%%	Template(+Vars:pairs, -Formats:list, -Args:list)//
%
%   Parse a list of codes as if it were an interpolated string. Formats
%   is a list of atoms that can be joined together to
%   create the first argument of format/2. Args are values for the
%   second.
template(Vars, [Static,_|Formats], [Arg|Args]) -->
    string(Codes),
    variable(VarName),
    { memberchk(VarName=Arg, Vars) },
    { atom_codes(Static, Codes) },
    template(Vars, Formats, Args).
template(_, [Format], []) -->
    string(Codes),
    { atom_codes(Format, Codes) }.


%%	variable(-VarName:atom)//
%
%	Parse a $-prefixed variable name.  Like `$Message`.
%   For now this is a thin wrapper around prolog_var_name//1 but may
%   eventuall grow to support other kinds of variables so I want the
%   details abstracted away.
variable(VarName) -->
    "$",
    prolog_var_name(VarName).


% true if the module whose terms are being read has specifically
% requested string interpolation.
wants_interpolation :-
    prolog_load_context(module, Module),
    Module \== interpolate,  % we don't want string interpolation ourselves
    predicate_property(
        Module:'$interpolate_macro_sentinel',
        imported_from(interpolate)
    ).


%%	textual(-Type:atom, +Text, -Codes)
%
%	True if Text is of type Type and representable as Codes.
textual(_,Var,_) :-
    var(Var),
    !,
    fail.
textual(atom, Atom, Codes) :-
    atom(Atom),
    !,
    atom_codes(Atom,Codes).
textual(Type, Text, Codes) :-
    is_list(Text),
    !,
    Text = [H|_],  % empty lists are not text for our purposes
    ( atom(H) ->
        Type = chars,
        catch(atom_chars(Atom,Text),_,fail),
        atom_codes(Atom,Codes)
    ; integer(H) ->
        Type = codes,
        Codes = Text
    ).  % fail on all other lists
textual(string, String, Codes) :-
    string(String),
    string_to_list(String,Codes).


%%	build_text(?Output, ?Formats:list, ?Args:list)
%
%   Like format/3 but dynamically chooses tilde sequences to match the
%   values in Args.
build_text(Output, Formats0, Args) :-
    instantiate_formats(Formats0, Args, Formats),
    atomic_list_concat(Formats, Format),
    format(Output, Format, Args).


% choose format tilde sequences for a list of values
instantiate_formats([], _, []).
instantiate_formats([Static|Formats0],Args,[Static|Formats]) :-
    atom(Static),
    !,
    instantiate_formats(Formats0,Args,Formats).
instantiate_formats([Var|Formats0],[Arg|Args],[Format|Formats]) :-
    var(Var),
    !,
    preferred_tilde(Arg,Format),
    instantiate_formats(Formats0,Args,Formats).
instantiate_formats([X|_],_,[_|_]) :-
    type_error(atom_or_var,X).


% Which format/2 tilde sequence does a value prefer?
preferred_tilde(X,'~s') :-
    textual(Type, X, _),
    ( Type = codes; Type = chars ),
    !.
preferred_tilde(_,'~p').


:- multifile user:function_expansion/3.
user:function_expansion(Term,Replacement,Guard) :-
    wants_interpolation,
    prolog_load_context(variable_names, Vars),
    Vars \== [],  % need variables to interpolate

    % is this a string in need of interpolation?
    textual(Type, Term, TextCodes),
    phrase(template(Vars, Formats, Args), TextCodes),
    Args \== [],  % no args means no interpolation

    % yup, so perform the expansion
    Output =.. [Type, Replacement],
    Guard = interpolate:build_text(Output, Formats, Args).


%%	'$interpolate_macro_sentinel'.
%
%   Nothing to see here. This is an implementation detail to avoid
%   unwanted string interpolation. Because string interpolation is
%   implemented as a macro and macro expansion is done through a
%   predicate with gloabl reach (user:term_expansion/2), we must take
%   precautions to avoid performing string interpolation on code that
%   doesn't want it. Importing this predicate opts-in to string
%   interpolation. Writing `use_module(library(interpolate))` does it
%   for you.
'$interpolate_macro_sentinel'.
