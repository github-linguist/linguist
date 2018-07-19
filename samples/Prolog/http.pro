:- use_module(library( http/http_open )).

http :-
	http_open('http://www.rosettacode.org/',In, []),
	copy_stream_data(In, user_output),
	close(In).
