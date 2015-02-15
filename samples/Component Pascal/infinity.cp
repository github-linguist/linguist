MODULE Infinity;
IMPORT StdLog;

PROCEDURE Do*;
VAR
	x: REAL;
BEGIN
	x := 1 / 0;
	StdLog.String("x:> ");StdLog.Real(x);StdLog.Ln
END Do;
