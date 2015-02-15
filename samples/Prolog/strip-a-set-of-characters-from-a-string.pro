:- use_module(library(lambda)).

stripchars(String, Exclude, Result) :-
	exclude(\X^(member(X, Exclude)), String, Result1),
	string_to_list(Result, Result1).
