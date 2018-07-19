EmptyQ[a_] := If[Length[a] == 0, True, False]
SetAttributes[Push, HoldAll];[a_, elem_] := AppendTo[a, elem]
SetAttributes[Pop, HoldAllComplete];
Pop[a_] := If[EmptyQ[a], False, b = Last[a]; Set[a, Most[a]]; b]
Peek[a_] := If[EmptyQ[a], False, Last[a]]

Example use:
stack = {};Push[stack, 1]; Push[stack, 2]; Push[stack, 3]; Push[stack, 4];
Peek[stack]
->4
Pop[stack]
->4
Peek[stack]
->3
