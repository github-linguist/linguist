-module(acc_factory).
-export([loop/1,new/1]).

loop(N)->
    receive
        {P,I}->
            S =N+I, P!S, loop(S)
    end.

new(N)->
    P=spawn(acc_factory,loop,[N]),
    fun(I)->
            P!{self(),I},
            receive
                V-> V
            end
    end.
