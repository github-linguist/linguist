MODULE BbtPangramChecker;
IMPORT StdLog,DevCommanders,TextMappers;

PROCEDURE Check(str: ARRAY OF CHAR): BOOLEAN;
CONST
	letters = 26;
VAR
	i,j: INTEGER;
	status: ARRAY letters OF BOOLEAN;
	resp : BOOLEAN;
BEGIN
	FOR i := 0 TO LEN(status) -1 DO status[i] := FALSE END;
	
	FOR i := 0 TO LEN(str) -  1 DO
		j := ORD(CAP(str[i])) - ORD('A');
		IF (0 <= j) & (25 >= j) & ~status[j] THEN status[j] := TRUE END
	END;
	
	resp := TRUE;
	FOR i := 0 TO LEN(status) - 1 DO;
		resp := resp & status[i]
	END;
	RETURN resp;
END Check;

PROCEDURE Do*;
VAR
	params: DevCommanders.Par;
	s: TextMappers.Scanner;
BEGIN
	params := DevCommanders.par;
	s.ConnectTo(params.text);
	s.SetPos(params.beg);
	s.Scan;
	WHILE (~s.rider.eot) DO
		IF (s.type = TextMappers.char) & (s.char = '~') THEN
			RETURN
		ELSIF (s.type # TextMappers.string) THEN
			StdLog.String("Invalid parameter");StdLog.Ln
		ELSE
			StdLog.Char("'");StdLog.String(s.string + "' is pangram?:> ");
			StdLog.Bool(Check(s.string));StdLog.Ln
		END;
		s.Scan
	END
END Do;

END BbtPangramChecker.
