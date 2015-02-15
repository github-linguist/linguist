-module( undefined_values ).

-export( [task/0] ).

-record( a_record, {member_1, member_2} ).

task() ->
    Record = #a_record{member_1=a_value},
    io:fwrite( "Record member_1 ~p, member_2 ~p~n", [Record#a_record.member_1, Record#a_record.member_2] ),
    io:fwrite( "Member_2 is undefined ~p~n", [Record#a_record.member_2 =:= undefined] ).
