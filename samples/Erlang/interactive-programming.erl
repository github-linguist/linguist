$erl
1> F = fun(X,Y,Z) -> string:concat(string:concat(X,Z),string:concat(Z,Y)) end.
#Fun<erl_eval.18.105910772>
2> F("Rosetta", "Code", ":").
"Rosetta::Code"
