MODULE IntegerSequence;
IMPORT StdLog;

PROCEDURE Do*;
VAR
	i: INTEGER;
BEGIN
	FOR i := 0 TO MAX(INTEGER) DO;
		StdLog.Int(i)
	END;
	StdLog.Ln
END Do;

END IntegerSequence.
