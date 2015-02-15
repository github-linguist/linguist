MODULE EvenOdd;
IMPORT StdLog,Args,Strings;

PROCEDURE BitwiseOdd(i: INTEGER): BOOLEAN;
BEGIN
	RETURN 0 IN BITS(i)
END BitwiseOdd;

PROCEDURE Odd(i: INTEGER): BOOLEAN;
BEGIN
	RETURN (i MOD 2) # 0
END Odd;

PROCEDURE CongruenceOdd(i: INTEGER): BOOLEAN;
BEGIN
	RETURN ((i -1) MOD 2) = 0
END CongruenceOdd;

PROCEDURE Do*;
VAR
	p: Args.Params;
	i,done,x: INTEGER;
BEGIN
	Args.Get(p);
	StdLog.String("Builtin function: ");StdLog.Ln;i := 0;
	WHILE i < p.argc DO
		Strings.StringToInt(p.args[i],x,done);
		StdLog.String(p.args[i] + " is:> ");
		IF ODD(x) THEN StdLog.String("odd") ELSE StdLog.String("even") END;
		StdLog.Ln;INC(i)
	END;
	StdLog.String("Bitwise: ");StdLog.Ln;i:= 0;
	WHILE i < p.argc DO
		Strings.StringToInt(p.args[i],x,done);
		StdLog.String(p.args[i] + " is:> ");
		IF BitwiseOdd(x) THEN StdLog.String("odd") ELSE StdLog.String("even") END;
		StdLog.Ln;INC(i)
	END;
	StdLog.String("Module: ");StdLog.Ln;i := 0;
	WHILE i < p.argc DO
		Strings.StringToInt(p.args[i],x,done);
		StdLog.String(p.args[i] + " is:> ");
		IF Odd(x) THEN StdLog.String("odd") ELSE StdLog.String("even") END;
		StdLog.Ln;INC(i)
	END;
	StdLog.String("Congruences: ");StdLog.Ln;i := 0;
	WHILE i < p.argc DO
		Strings.StringToInt(p.args[i],x,done);
		StdLog.String(p.args[i] + " is:> ");
		IF CongruenceOdd(x) THEN StdLog.String("odd") ELSE StdLog.String("even") END;
		StdLog.Ln;INC(i)
	END;
END Do;
