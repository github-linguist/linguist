MODULE LetterFrecuency;
IMPORT Files,StdLog,Strings;

PROCEDURE Do*;
VAR
	loc: Files.Locator;
	fd: Files.File;
	rd: Files.Reader;
	x: BYTE;
	frecuency: ARRAY 26 OF LONGINT;
	c: CHAR;
	i: INTEGER;
BEGIN
	loc := Files.dir.This("BBTest/Mod");
	fd := Files.dir.Old(loc,"LetterFrecuency.odc",FALSE);
	rd := fd.NewReader(NIL);
	
	(* init the frecuency array *)
	FOR i := 0 TO LEN(frecuency) - 1 DO frecuency[i] := 0 END;
	
	(* collect frecuencies *)
	WHILE ~rd.eof DO
		rd.ReadByte(x);c := CAP(CHR(x));
		(* convert vowels with diacritics *)
		CASE ORD(c) OF
			 193: c := 'A';
			|201: c := 'E';
			|205: c := 'I';
			|211: c := 'O';
			|218: c := 'U';
			ELSE
		END;
		IF (c >= 'A') & (c <= 'Z') THEN
			INC(frecuency[ORD(c) - ORD('A')]);
		END
	END;
	
	(* show data *)
	FOR i := 0 TO LEN(frecuency) - 1 DO
		StdLog.Char(CHR(i + ORD('A')));StdLog.String(":> ");StdLog.Int(frecuency[i]);
		StdLog.Ln
	END
END Do;
END LetterFrecuency.
