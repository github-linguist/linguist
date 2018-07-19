MODULE BinaryDigits;
IMPORT StdLog,Strings;

PROCEDURE Do*;
VAR
	str : ARRAY 33 OF CHAR;
BEGIN
	Strings.IntToStringForm(5,2,32,'0',FALSE,str);
	StdLog.Int(5);StdLog.String(":> " + str);StdLog.Ln;
	Strings.IntToStringForm(50,2,32,'0',FALSE,str);
	StdLog.Int(50);StdLog.String(":> " + str);StdLog.Ln;
	Strings.IntToStringForm(9000,2,32,'0',FALSE,str);
	StdLog.Int(9000);StdLog.String(":> " + str);StdLog.Ln;
END Do;
END BinaryDigits.
