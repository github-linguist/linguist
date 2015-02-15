MODULE ArrayConcat;
IMPORT StdLog;

PROCEDURE Concat(x: ARRAY OF INTEGER; y: ARRAY OF INTEGER; OUT z: ARRAY OF INTEGER);
VAR
	i: INTEGER;
BEGIN
	ASSERT(LEN(x) + LEN(y) <= LEN(z));
	FOR i := 0 TO LEN(x) - 1 DO z[i] := x[i] END;
	FOR i := 0 TO LEN(y) - 1 DO z[i + LEN(x)] := y[i] END
END Concat;

PROCEDURE Concat2(x: ARRAY OF INTEGER;y: ARRAY OF INTEGER): POINTER TO ARRAY OF INTEGER;
VAR
	z: POINTER TO ARRAY OF INTEGER;
	i: INTEGER;
BEGIN
	NEW(z,LEN(x) + LEN(y));
	FOR i := 0 TO LEN(x) - 1 DO z[i] := x[i] END;
	FOR i := 0 TO LEN(y) - 1 DO z[i + LEN(x)] := y[i] END;
	RETURN z;
END Concat2;

PROCEDURE ShowArray(x: ARRAY OF INTEGER);
VAR
	i: INTEGER;
BEGIN
	i := 0;
	StdLog.Char('[');
	WHILE (i < LEN(x)) DO
		StdLog.Int(x[i]);IF i < LEN(x) - 1 THEN StdLog.Char(',') END;
		INC(i)
	END;
	StdLog.Char(']');StdLog.Ln;
END ShowArray;
	
PROCEDURE Do*;
VAR
	x: ARRAY 10 OF INTEGER;
	y: ARRAY 15 OF INTEGER;
	z: ARRAY 25 OF INTEGER;
	w: POINTER TO ARRAY OF INTEGER;
	i: INTEGER;
BEGIN
	FOR i := 0 TO LEN(x) - 1 DO x[i] := i END;
	FOR i := 0 TO LEN(y) - 1 DO y[i] := i END;
	Concat(x,y,z);StdLog.String("1> ");ShowArray(z);
	
	NEW(w,LEN(x) + LEN(y));
	Concat(x,y,z);StdLog.String("2:> ");ShowArray(z);
	
	StdLog.String("3:> ");ShowArray(Concat2(x,y));
END Do;

END ArrayConcat.
