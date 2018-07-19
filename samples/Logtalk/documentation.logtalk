:- object(set(_Type),
	extends(set)).

	% the info/1 directive is the main directive for documenting an entity
	% its value is a list of Key-Value pairs; the set of keys is user-extendable
	:- info([
		version is 1.2,
		author is 'A. Coder',
		date is 2013/10/13,
		comment is 'Set predicates with elements constrained to a single type.',
		parnames is ['Type']
	]).

	% the info/2 directive is the main directive for documenting predicates
	% its second value is a list of Key-Value pairs; the set of keys is user-extendable
	:- public(intersection/3).
	:- mode(intersection(+set, +set, ?set), zero_or_one).
	:- info(intersection/3, [
		comment is 'Returns the intersection of Set1 and Set2.',
		argnames is ['Set1', 'Set2', 'Intersection']
	]).

	...

:- end_object.
