(* ::Package:: *)

BeginPackage["ExamplePackage`"]


(* Public ussage, but define function private *)


intSpF::usage = "mySmallTestF[f,a,b]

integrates f from a to b and divides by the length of the interval. Does point evaluation of a equals b"; 


	Begin["`Private`"];


    intSpF[f_,a_,b_] := 1/(b-a)*Integrate[f[x],{x,a,b}] /;(a != b)


    intSpF[f_,a_,b_] := f[a] /;(a == b)


	End[]


End[]
