MODULE MainProcedure;
IMPORT StdLog;

PROCEDURE Do*;
BEGIN
	StdLog.String("From Do");StdLog.Ln
END Do;

PROCEDURE Main*;
BEGIN
	StdLog.String("From Main");StdLog.Ln
END Main;
END MainProcedure.
