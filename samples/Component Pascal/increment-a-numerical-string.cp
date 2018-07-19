MODULE Operations;
IMPORT StdLog,Args,Strings;

PROCEDURE IncString(s: ARRAY OF CHAR): LONGINT;
VAR
	resp: LONGINT;
	done: INTEGER;
BEGIN
	Strings.StringToLInt(s,resp,done);
	INC(resp);
	RETURN resp
END IncString;

PROCEDURE DoIncString*;
VAR
	p: Args.Params;
BEGIN
	Args.Get(p);
	IF p.argc > 0 THEN
		StdLog.String(p.args[0] + " + 1= ");StdLog.Int(IncString(p.args[0]));StdLog.Ln
	END
END DoIncString;

END Operations.
