(* ::Package:: *)

BeginPackage["Predicates`"];


(* ::Title:: *)
(*Predicates*)


(* ::Section::Closed:: *)
(*Fuzzy Logic*)


(* ::Subsection:: *)
(*Documentation*)


PossiblyTrueQ::usage="Returns True if the argument is not definitely False.";
PossiblyFalseQ::usage="Returns True if the argument is not definitely True.";
PossiblyNonzeroQ::usage="Returns True if and only if its argument is not definitely zero.";


(* ::Subsection:: *)
(*Implimentation*)


Begin["`Private`"];


PossiblyTrueQ[expr_]:=\[Not]TrueQ[\[Not]expr]


PossiblyFalseQ[expr_]:=\[Not]TrueQ[expr]


End[];


(* ::Section::Closed:: *)
(*Numbers and Lists*)


(* ::Subsection:: *)
(*Documentation*)


AnyQ::usage="Given a predicate and a list, retuns True if and only if that predicate is True for at least one element of the list.";
AnyElementQ::usage="Returns True if cond matches any element of L.";
AllQ::usage="Given a predicate and a list, retuns True if and only if that predicate is True for all elements of the list.";
AllElementQ::usage="Returns True if cond matches any element of L.";


AnyNonzeroQ::usage="Returns True if L is a list such that at least one element is definitely not zero.";
AnyPossiblyNonzeroQ::usage="Returns True if expr is a list such that at least one element is not definitely zero.";


RealQ::usage="Returns True if and only if the argument is a real number";
PositiveQ::usage="Returns True if and only if the argument is a positive real number";
NonnegativeQ::usage="Returns True if and only if the argument is a non-negative real number";
PositiveIntegerQ::usage="Returns True if and only if the argument is a positive integer";
NonnegativeIntegerQ::usage="Returns True if and only if the argument is a non-negative integer";


IntegerListQ::usage="Returns True if and only if the input is a list of integers.";
PositiveIntegerListQ::usage="Returns True if and only if the input is a list of positive integers.";
NonnegativeIntegerListQ::usage="Returns True if and only if the input is a list of non-negative integers.";
IntegerOrListQ::usage="Returns True if and only if the input is a list of integers or an integer.";
PositiveIntegerOrListQ::usage="Returns True if and only if the input is a list of positive integers or a positive integer.";
NonnegativeIntegerOrListQ::usage="Returns True if and only if the input is a list of positive integers or a positive integer.";


SymbolQ::usage="Returns True if argument is an unassigned symbol.";
SymbolOrNumberQ::usage="Returns True if argument is a number of has head 'Symbol'";


(* ::Subsection:: *)
(*Implimentation*)


Begin["`Private`"];


AnyQ[cond_, L_] := Fold[Or, False, cond /@ L]


AnyElementQ[cond_,L_]:=AnyQ[cond,Flatten[L]]


AllQ[cond_, L_] := Fold[And, True, cond /@ L]


AllElementQ[cond_, L_] := Fold[And, True, cond /@ L]


AnyNonzeroQ[L_]:=AnyElementQ[#!=0&,L]


PossiblyNonzeroQ[expr_]:=PossiblyTrueQ[expr!=0]


AnyPossiblyNonzeroQ[expr_]:=AnyElementQ[PossiblyNonzeroQ,expr]


RealQ[n_]:=TrueQ[Im[n]==0];


PositiveQ[n_]:=Positive[n];


PositiveIntegerQ[n_]:=PositiveQ[n]\[And]IntegerQ[n];


NonnegativeQ[n_]:=TrueQ[RealQ[n]&&n>=0];


NonnegativeIntegerQ[n_]:=NonnegativeQ[n]\[And]IntegerQ[n];


IntegerListQ[input_]:=ListQ[input]&&Not[MemberQ[IntegerQ/@input,False]];


IntegerOrListQ[input_]:=IntegerListQ[input]||IntegerQ[input];


PositiveIntegerListQ[input_]:=IntegerListQ[input]&&Not[MemberQ[Positive[input],False]];


PositiveIntegerOrListQ[input_]:=PositiveIntegerListQ[input]||PositiveIntegerQ[input];


NonnegativeIntegerListQ[input_]:=IntegerListQ[input]&&Not[MemberQ[NonnegativeIntegerQ[input],False]];


NonnegativeIntegerOrListQ[input_]:=NonnegativeIntegerListQ[input]||NonnegativeIntegerQ[input];


SymbolQ[a_]:=Head[a]===Symbol;


SymbolOrNumberQ[a_]:=NumericQ[a]||Head[a]===Symbol;


End[];


(* ::Section:: *)
(*Epilogue*)


EndPackage[];
