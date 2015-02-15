-module( introspection ).

-export( [task/0] ).

task() ->
    exit_if_too_old( erlang:system_info(otp_release) ),
    Bloop = lists:keyfind( bloop, 1, ?MODULE:module_info(functions) ),
    Abs = lists:keyfind( abs, 1, erlang:module_info(exports) ),
    io:fwrite( "abs( bloop ) => ~p~n", [call_abs_with_bloop(Abs, Bloop)] ),
    io:fwrite( "Number of modules: ~p~n", [erlang:length(code:all_loaded())] ).



bloop() -> -1.

call_abs_with_bloop( {abs, 1}, {bloop, 0} ) -> erlang:abs( bloop() );
call_abs_with_bloop( _Missing, _Not_here ) -> abs_and_bloop_missing.

exit_if_too_old( Release ) when Release	< "R13A" -> erlang:exit( too_old_release );
exit_if_too_old( _Release ) -> ok.
