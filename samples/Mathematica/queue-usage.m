Empty[a_] := If[Length[a] == 0, True, False]
SetAttributes[Push, HoldAll]; Push[a_, elem_] := AppendTo[a, elem]
SetAttributes[Pop, HoldAllComplete]; Pop[a_] := If[EmptyQ[a], False, b = First[a]; Set[a, Most[a]]; b]

Queue = {}
-> {}
Empty[Queue]
-> True
Push[Queue, "1"]
-> {"1"}
EmptyQ[Queue]
->False
Pop[Queue]
->1
Pop[Queue]
->False
