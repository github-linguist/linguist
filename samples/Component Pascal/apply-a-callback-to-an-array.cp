MODULE Callback;
IMPORT StdLog;

TYPE
	Callback = PROCEDURE (x: INTEGER;OUT doubled: INTEGER);
	Callback2 = PROCEDURE (x: INTEGER): INTEGER;
	
	PROCEDURE Apply(proc: Callback; VAR x: ARRAY OF INTEGER);
	VAR
		i: INTEGER;
	BEGIN
		FOR i := 0 TO LEN(x) - 1 DO;
			proc(x[i],x[i]);
		END
	END Apply;
	
	PROCEDURE Apply2(func: Callback2; VAR x: ARRAY OF INTEGER);
	VAR
		i: INTEGER;
	BEGIN
		FOR i := 0 TO LEN(x) - 1 DO;
			x[i] := func(x[i]);
		END
	END Apply2;
	
	PROCEDURE Double(x: INTEGER; OUT y: INTEGER);
	BEGIN	
		y := x * x;
	END Double;
	
	PROCEDURE Double2(x: INTEGER): INTEGER;
	BEGIN
		RETURN x * x
	END Double2;
	
	PROCEDURE Do*;
	VAR
		i: INTEGER;
		ary: ARRAY 10 OF INTEGER;
		
		
	BEGIN
		FOR i := 0 TO LEN(ary) - 1 DO ary[i] := i END;
		Apply(Double,ary);
		FOR i := 0 TO LEN(ary) - 1 DO
			StdLog.Int(ary[i]);StdLog.Ln
		END;
		StdLog.Ln;
		Apply2(Double2,ary);
		FOR  i := 0 TO LEN(ary) - 1 DO
		        StdLog.Int(ary[i]);StdLog.Ln
		END
	END Do;
END Callback.
