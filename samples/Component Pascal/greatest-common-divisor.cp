MODULE Operations;
IMPORT StdLog,Args,Strings;

PROCEDURE Gcd(a,b: LONGINT):LONGINT;
VAR
	r: LONGINT;
BEGIN
	LOOP
		r := a MOD b;
		IF r = 0 THEN RETURN b END;
		a := b;b := r
	END
END Gcd;

PROCEDURE DoGcd*;
VAR
	x,y,done: INTEGER;
	p: Args.Params;
BEGIN
	Args.Get(p);
	IF p.argc >= 2 THEN
		Strings.StringToInt(p.args[0],x,done);
		Strings.StringToInt(p.args[1],y,done);
		StdLog.String("gcd("+p.args[0]+","+p.args[1]+")=");StdLog.Int(Gcd(x,y));StdLog.Ln
	END		
END DoGcd;

END Operations.
