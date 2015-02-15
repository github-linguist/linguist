MODULE StringConcatenation;
IMPORT StdLog;

PROCEDURE Do*;
VAR
	str1,str2: ARRAY 128 OF CHAR;
BEGIN
	str1 := "Hello";
	str2 := str1 + " world";
	StdLog.String(":> " + str2);StdLog.Ln
END Do;

END StringConcatenation.
