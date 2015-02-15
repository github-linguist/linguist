-module( rendezvous ).

-export( [task/0] ).

task() ->
    Printer_pid = erlang:spawn( fun() -> printer(1, 5) end ),
    Reserve_printer_pid = erlang:spawn( fun() -> printer(2, 5) end ),
    Monitor_pid = erlang:spawn( fun() -> printer_monitor(Printer_pid, Reserve_printer_pid) end ),
    erlang:spawn( fun() -> print(Monitor_pid, humpty_dumpty()) end ),
    erlang:spawn( fun() -> print(Monitor_pid, mother_goose()) end ).

humpty_dumpty() ->
        ["Humpty Dumpty sat on a wall.",
                "Humpty Dumpty had a great fall.",
                "All the king's horses and all the king's men,",
                "Couldn't put Humpty together again."].

mother_goose() ->
        ["Old Mother Goose,",
               "When she wanted to wander,",
               "Would ride through the air,",
               "On a very fine gander.",
               "Jack's mother came in,",
               "And caught the goose soon,",
               "And mounting its back,",
               "Flew up to the moon."].

print( Pid, Lines ) ->
    io:fwrite( "Print ~p started~n", [erlang:self()] ),
    print( Pid, Lines, infinity ).

print( _Pid, [], _Timeout ) -> ok;
print( Pid, [Line | T], Timeout ) ->
    print_line( Pid, Line, Timeout ),
    print_line_done(),
    print( Pid, T, Timeout ).

print_line( Pid, Line, Timeout ) ->
    Pid ! {print, Line, erlang:self()},
    receive
    {print, started} -> ok
    after Timeout -> erlang:throw( timeout )
    end.

print_line_done() ->
    receive
    {printer, ok} -> ok;
    {printer, out_of_ink} -> erlang:throw( out_of_ink )
    end.

printer( N, 0 ) ->
        receive
        {print, _Line, Pid} -> Pid ! {printer, out_of_ink}
        end,
        printer( N, 0 );
printer( N, Ink ) ->
        receive
        {print, Line, Pid} ->
                Pid ! {printer, ok},
                io:fwrite( "~p: ", [N] ),
                [io:fwrite("~c", [X]) || X <- Line],
                io:nl()
        end,
        printer( N, Ink - 1 ).

printer_monitor( Printer, Reserve ) ->
        {Line, Pid} = printer_monitor_get_line(),
        Result = printer_monitor_print_line( Printer, Line ),
        printer_monitor_reserve( Result, Reserve, Line, Pid ),
        printer_monitor( Printer, Reserve ).

printer_monitor_get_line() ->
        receive
        {print, Line, Pid} ->
                Pid ! {print, started},
                {Line, Pid}
        end.

printer_monitor_print_line( Printer_pid, Line ) ->
        Printer_pid ! {print, Line, erlang:self()},
        receive
        {printer, Result} -> Result
        end.

printer_monitor_reserve( ok, _Reserve_pid, _Line, Pid ) -> Pid ! {printer, ok};
printer_monitor_reserve( out_of_ink, Reserve_pid, Line, Pid ) -> Reserve_pid ! {print, Line, Pid}.
