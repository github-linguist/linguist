f[x_] := If[x > 10, Throw[overflow], x!]

Example usage :
Catch[f[2] + f[11]]
-> overflow

Catch[f[2] + f[3]]
-> 8
