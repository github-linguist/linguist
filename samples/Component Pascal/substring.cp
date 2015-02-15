MODULE Substrings;
IMPORT StdLog,Strings;

PROCEDURE Do*;
CONST
	aStr = "abcdefghijklmnopqrstuvwxyz";
VAR
	str: ARRAY 128 OF CHAR;
	pos: INTEGER;
BEGIN
	Strings.Extract(aStr,3,10,str);
	StdLog.String("from 3, 10 characters:> ");StdLog.String(str);StdLog.Ln;
	Strings.Extract(aStr,3,LEN(aStr) - 3,str);
	StdLog.String("from 3, until the end:> ");StdLog.String(str);StdLog.Ln;
	Strings.Extract(aStr,0,LEN(aStr) - 1,str);
	StdLog.String("whole string but last:> ");StdLog.String(str);StdLog.Ln;
	Strings.Find(aStr,'d',0,pos);
	Strings.Extract(aStr,pos + 1,10,str);
	StdLog.String("from 'd', 10 characters:> ");StdLog.String(str);StdLog.Ln;
	Strings.Find(aStr,"de",0,pos);
	Strings.Extract(aStr,pos + LEN("de"),10,str);
	StdLog.String("from 'de', 10 characters:> ");StdLog.String(str);StdLog.Ln;
END Do;

END Substrings.
