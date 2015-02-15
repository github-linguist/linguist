MODULE TestArray;
(* Implemented in BlackBox Component Builder *)

	IMPORT Out;

	(* Open array *)
	
	PROCEDURE DoTwoDim*;
		VAR d: POINTER TO ARRAY OF ARRAY OF INTEGER;
	BEGIN
		NEW(d, 5, 4); (* allocating array in memory *)
		d[1, 2] := 100; (* second row, third column element *)
		d[4, 3] := -100; (* fifth row, fourth column element *)
		Out.Int(d[1, 2], 0); Out.Ln;
		Out.Int(d[4, 3], 0); Out.Ln;
	END DoTwoDim;

END TestArray.
