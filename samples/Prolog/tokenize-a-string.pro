splitup(Sep,[token(B)|BL]) --> splitup(Sep,B,BL).
splitup(Sep,[A|AL],B)      --> [A], {\+ [A] = Sep }, splitup(Sep,AL,B).
splitup(Sep,[],[B|BL])     --> Sep, splitup(Sep,B,BL).
splitup(_Sep,[],[])        --> [].
start :-
    phrase(splitup(",",Tokens),"Hello,How,Are,You,Today"),
    phrase(splitup(".",Tokens),Backtogether),
    string_to_list(ABack,Backtogether),
    writeln(ABack).
