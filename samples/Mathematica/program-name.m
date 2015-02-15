#!/usr/bin/env MathKernel -script

ScriptName[] = Piecewise[
	{
		{"Interpreted", Position[$CommandLine, "-script", 1] == {}}
	},
	$CommandLine[[Position[$CommandLine, "-script", 1][[1,1]] + 1]]
]

Program = ScriptName[];

Print["Program: " <> Program]
