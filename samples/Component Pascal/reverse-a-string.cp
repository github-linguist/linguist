MODULE BbtReverseString;
IMPORT StdLog;

PROCEDURE ReverseStr(str: ARRAY OF CHAR): POINTER TO ARRAY OF CHAR;
VAR
	top,middle,i: INTEGER;
	c: CHAR;
	rStr: POINTER TO ARRAY OF CHAR;
BEGIN
	NEW(rStr,LEN(str$) + 1);
	top := LEN(str$) - 1; middle := (top - 1) DIV 2;
	FOR i := 0 TO middle DO
		rStr[i] := str[top - i];
		rStr[top - i] := str[i];
	END;
	IF ODD(LEN(str$)) THEN rStr[middle + 1] := str[middle + 1] END;
	RETURN rStr;
END ReverseStr;

PROCEDURE Do*;
VAR
	x: CHAR;
BEGIN
	StdLog.String("'asdf' reversed:> ");StdLog.String(ReverseStr("asdf"));StdLog.Ln
END Do;
END BbtReverseString.
