EmptyDirectoryQ[x_] := (SetDirectory[x]; If[FileNames[] == {}, True, False])

Example use:
EmptyDirectoryQ["C:\\Program Files\\Wolfram Research\\Mathematica\\9"]
->True
