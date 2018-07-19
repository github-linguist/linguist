MODULE AlphaBeta;
IMPORT StdLog,Strings;

PROCEDURE Do*;
VAR
	str,res: ARRAY 128 OF CHAR;
BEGIN
	str := "alphaBETA";
	Strings.ToUpper(str,res);
	StdLog.String("Uppercase:> ");StdLog.String(res);StdLog.Ln;
	Strings.ToLower(str,res);
	StdLog.String("Lowercase:> ");StdLog.String(res);StdLog.Ln
END Do;

END AlphaBeta.
