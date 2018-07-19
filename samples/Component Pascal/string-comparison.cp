MODULE StringComparision;
IMPORT StdLog,Strings;

PROCEDURE Do*;
VAR
	str1,str2,aux1,aux2: ARRAY 128 OF CHAR;
BEGIN
	str1 := "abcde";str2 := "abcde";
	StdLog.String(str1+" equals " + str2  + ":> ");StdLog.Bool(str1 = str2);StdLog.Ln;
	str2 := "abcd";
	StdLog.String(str1+" equals " + str2  + ":> ");StdLog.Bool(str1 = str2);StdLog.Ln;
	StdLog.String(str1+" greater than " + str2  + ":> ");StdLog.Bool(str1 > str2);StdLog.Ln;
	StdLog.String(str1+" lower than " + str2  + ":> ");StdLog.Bool(str1 < str2);StdLog.Ln;
	
	str2 := "ABCDE";
	StdLog.String(str1+" equals " + str2  + ":> ");StdLog.Bool(str1 = str2);StdLog.Ln;
	StdLog.String(str1+" greater than " + str2  + ":> ");StdLog.Bool(str1 > str2);StdLog.Ln;
	StdLog.String(str1+" lower than " + str2  + ":> ");StdLog.Bool(str1 < str2);StdLog.Ln;
	
	Strings.ToLower(str1,aux1);Strings.ToLower(str2,aux2);
	StdLog.String(str1+" equals (case insensitive) " + str2  + ":> ");StdLog.Bool(aux1 = aux2);StdLog.Ln;
	
	str1 := "01234";str2 := "01234";
	StdLog.String(str1+" equals " + str2  + ":> ");StdLog.Bool(str1 = str2);StdLog.Ln;
	str2 := "0123";
	StdLog.String(str1+" equals " + str2  + ":> ");StdLog.Bool(str1 = str2);StdLog.Ln;
	StdLog.String(str1+" greater than " + str2  + ":> ");StdLog.Bool(str1 > str2);StdLog.Ln;
	StdLog.String(str1+" lower than " + str2  + ":> ");StdLog.Bool(str1 < str2);StdLog.Ln;
END Do;

END StringComparision.
