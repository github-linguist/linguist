PROCEDURE CharCodes*;
VAR
	c : CHAR;
BEGIN
	c := 'A';
	StdLog.Char(c);StdLog.String(":> ");StdLog.Int(ORD(c));StdLog.Ln;
	c := CHR(3A9H);
	StdLog.Char(c);StdLog.String(":> ");StdLog.Int(ORD(c));StdLog.Ln
END CharCodes;
