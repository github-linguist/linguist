MODULE Algebras;
IMPORT StdLog,Strings;

TYPE
	Matrix = POINTER TO ARRAY OF ARRAY OF INTEGER;
	
PROCEDURE NewIdentityMatrix(n: INTEGER): Matrix;
VAR
	m: Matrix;
	i: INTEGER;
BEGIN
	NEW(m,n,n);
	FOR i := 0 TO n - 1 DO
		m[i,i] := 1;
	END;
	RETURN m;
END NewIdentityMatrix;

PROCEDURE Show(m: Matrix);
VAR
	i,j: INTEGER;
BEGIN
	FOR i := 0 TO LEN(m,0) - 1 DO
		FOR j := 0 TO LEN(m,1) - 1 DO
			StdLog.Int(m[i,j]);
		END;
		StdLog.Ln
	END
END Show;

PROCEDURE Do*;
BEGIN
	Show(NewIdentityMatrix(5));
END Do;
END Algebras.
