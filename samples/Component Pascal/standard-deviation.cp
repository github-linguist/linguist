MODULE StandardDeviation;
IMPORT StdLog, Args,Strings,Math;

PROCEDURE Mean(x: ARRAY OF REAL; n: INTEGER; OUT mean: REAL);
VAR
	i: INTEGER;
	total: REAL;
BEGIN
	total := 0.0;
	FOR i := 0 TO n - 1 DO total := total + x[i] END;
	mean := total /n
END Mean;

PROCEDURE SDeviation(x : ARRAY OF REAL;n: INTEGER): REAL;
VAR
	i: INTEGER;
	mean,sum: REAL;
BEGIN
	Mean(x,n,mean);
	sum := 0.0;
	FOR i := 0 TO n - 1 DO
		sum:= sum +  ((x[i] - mean) * (x[i] - mean));
	END;
	RETURN Math.Sqrt(sum/n);
END SDeviation;

PROCEDURE Do*;
VAR
	p: Args.Params;
	x: POINTER TO ARRAY OF REAL;
	i,done: INTEGER;
BEGIN
	Args.Get(p);
	IF p.argc > 0 THEN
		NEW(x,p.argc);
		FOR i := 0 TO p.argc - 1 DO x[i] := 0.0 END;
		FOR i  := 0 TO p.argc - 1 DO
			Strings.StringToReal(p.args[i],x[i],done);
			StdLog.Int(i + 1);StdLog.String(" :> ");StdLog.Real(SDeviation(x,i + 1));StdLog.Ln
		END
	END
END Do;
END StandardDeviation.
