MODULE AddressVar;
IMPORT SYSTEM,StdLog;

VAR
	x: INTEGER;
	
PROCEDURE Do*;
BEGIN
	StdLog.String("ADR(x):> ");StdLog.IntForm(SYSTEM.ADR(x),StdLog.hexadecimal,8,'0',TRUE);StdLog.Ln
END Do;

BEGIN
	x := 10;
END AddressVar.
