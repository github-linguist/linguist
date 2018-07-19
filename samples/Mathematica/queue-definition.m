EmptyQ[a_] := Length[a] == 0
SetAttributes[Push, HoldAll]; Push[a_, elem_] := AppendTo[a, elem]
SetAttributes[Pop, HoldAllComplete]; Pop[a_] := If[EmptyQ[a], False, b = First[a]; Set[a, Most[a]]; b]
