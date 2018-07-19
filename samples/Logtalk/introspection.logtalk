:- object(my_application).

    :- initialization((
        check_logtalk_version,
        compute_predicate_if_available
    )).

    check_logtalk_version :-
        % version data is available by consulting the "version_data" flag
        current_logtalk_flag(version_data, logtalk(Major,Minor,Patch,_)),
        (   (Major,Minor,Patch) @< (3,0,0) ->
            write('Logtalk version is too old! Please upgrade to version 3.0.0 or later'), nl,
            halt
        ;   true
        ).

    % Logtalk is not a functional language and thus doesn't support user-defined functions; we
    % use instead a predicate, abs/2, with a return argument to implement the abs/1 function
    compute_predicate_if_available :-
        (   % check that the variable "bloop" is defined within this object
            current_predicate(bloop/1),
            % assume that the abs/2 predicate, if available, comes from a "utilities" object
            utilities::current_predicate(abs/2) ->
            bloop(Value),
            utilities::abs(Value, Result),
            write('Function value: '), write(Result), nl
        ;   write('Could not compute function!'), nl
        ).

    % our "bloop" variable value as per task description
    bloop(-1).

:- end_object.
