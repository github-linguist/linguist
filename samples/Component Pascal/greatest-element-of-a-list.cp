MODULE Operations;
IMPORT StdLog,Args,Strings;

PROCEDURE Max(s: ARRAY OF INTEGER): INTEGER;
VAR
	i: INTEGER;
	max: INTEGER;
BEGIN
	max := MIN(INTEGER);
	FOR i := 0 TO LEN(s) - 1 DO
		max := MAX(max,s[i]);
	END;
	RETURN max
END Max;

PROCEDURE DoMax*;
VAR	
	sq: POINTER TO ARRAY OF INTEGER;
	p: Args.Params;
	i,n,done: INTEGER;
BEGIN
	Args.Get(p);
	IF p.argc > 0 THEN
		NEW(sq,p.argc);
		FOR i := 0 TO p.argc - 1 DO
			Strings.StringToInt(p.args[i],n,done);
			sq[i] := n
		END;
		StdLog.String("max:> ");StdLog.Int(Max(sq));StdLog.Ln
	END
END DoMax;

END Operations.
