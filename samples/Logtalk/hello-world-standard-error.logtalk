:- object(error_message).

    % the initialization/1 directive argument is automatically executed
    % when the object is compiled and loaded into memory:
    :- initialization(write(user_error, 'Goodbye, World!\n')).

:- end_object.
