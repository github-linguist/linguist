(* ::Package:: *)

(* Problem12.m *)
(* Author: William Woodruff *)
(* Problem: What is the value of the first triangle number to have over five hundred divisors? *)

Do[If[Length[Divisors[Binomial[i + 1, 2]]] > 500, 
  Print[Binomial[i + 1, 2]]; Break[]], {i, 1000000}]
