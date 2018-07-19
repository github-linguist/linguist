| ?- create_object(Id, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
     write('Variable name:  '), read(Name),
     write('Variable value: '), read(Value),
     Fact =.. [Name, Value],
     Id::assertz(Fact).

Variable name:  foo.
Variable value: 42.
Id = o1,
Name = foo,
Value =  42,
Fact = foo(42).

?- o1::current_predicate(foo/1).
true.

| ?- o1::foo(X).
X = 42.
