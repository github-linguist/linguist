% this is a Logtalk source file

:- object(hello_world).

	% the initialization/1 directive argument is automatically executed
	% when the object is loaded into memory:
	:- initialization((nl, write('********** Hello World! **********'), nl)).

:- end_object.
