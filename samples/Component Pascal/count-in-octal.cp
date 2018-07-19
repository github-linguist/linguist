MODULE CountOctal;
IMPORT StdLog,Strings;

PROCEDURE Do*;
VAR
	i: INTEGER;
	resp: ARRAY 32 OF CHAR;
BEGIN
	FOR i := 0 TO 1000 DO
		Strings.IntToStringForm(i,8,12,' ',TRUE,resp);
		StdLog.String(resp);StdLog.Ln
	END
END Do;
END CountOctal.
