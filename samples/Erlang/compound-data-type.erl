-module(records_test).
-compile(export_all).

-record(point,{x,y}).

test() ->
    P1 = #point{x=1.0,y=2.0}, % creates a new point record
    io:fwrite("X: ~f, Y: ~f~n",[P1#point.x,P1#point.y]),
    P2 = P1#point{x=3.0}, % creates a new point record with x set to 3.0, y is copied from P1
    io:fwrite("X: ~f, Y: ~f~n",[P2#point.x,P2#point.y]).
