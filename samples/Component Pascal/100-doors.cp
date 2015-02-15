MODULE Doors100;
IMPORT StdLog;

PROCEDURE Do*;
VAR
	i,j: INTEGER;
	closed: ARRAY 101 OF BOOLEAN;
BEGIN
	(* initilization of close to true *)
	FOR i := 0 TO LEN(closed) - 1 DO closed[i] := TRUE END;
	(* process *)
	FOR i := 1 TO LEN(closed)  DO;
		j := 1;
		WHILE j < LEN(closed) DO
			IF j MOD i = 0 THEN closed[j] := ~closed[j] END;INC(j)
		END
	END;
	(* print results *)
	i := 1;
	WHILE  i < LEN(closed)  DO
		IF (i - 1) MOD 10 = 0 THEN StdLog.Ln END;
		IF closed[i] THEN StdLog.String("C ") ELSE StdLog.String("O ") END;
		INC(i)
	END;
END Do;
END Doors100.
