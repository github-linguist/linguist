MODULE TestArray;
(* Implemented in BlackBox Component Builder *)

	IMPORT Out;
	
	(* Static array *)
	
	PROCEDURE DoOneDim*;
		CONST M = 5;
		VAR a: ARRAY M OF INTEGER;
	BEGIN
		a[0] := 100; (* set first element's value of array a to 100 *)
		a[M-1] := -100; (* set M-th element's value of array a to -100 *)
		Out.Int(a[0], 0); Out.Ln;
		Out.Int(a[M-1], 0); Out.Ln;
	END DoOneDim;

	PROCEDURE DoTwoDim*;
		VAR b: ARRAY 5, 4 OF INTEGER;
	BEGIN
		b[1, 2] := 100; (* second row, third column element *)
		b[4, 3] := -100; (* fifth row, fourth column element *)
		Out.Int(b[1, 2], 0); Out.Ln;
		Out.Int(b[4, 3], 0); Out.Ln;
	END DoTwoDim;

END TestArray.
