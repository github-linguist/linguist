:- object(error_message).

    % the initialization/1 directive argument is automatically executed
    % when the object is compiled loaded into memory:
    :- initialization(write('Goodbye, World!')).

:- end_object.
