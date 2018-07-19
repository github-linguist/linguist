MODULE BottlesOfBeer;
IMPORT StdLog;
CONST bottles = 99;

PROCEDURE Part(i: INTEGER);
BEGIN
	StdLog.Int(i);StdLog.String(" bottles of beer on the wall");StdLog.Ln;
	StdLog.Int(i);StdLog.String(" bottles of beer");StdLog.Ln;
	StdLog.String("Take one down, pass it around");StdLog.Ln;
	StdLog.Int(i - 1);StdLog.String(" bottles of beer on the wall.");StdLog.Ln;
	StdLog.Ln
END Part;

PROCEDURE Sing*;
VAR
	i: INTEGER;
BEGIN
	FOR i := bottles TO 1 BY -1 DO
		Part(i)
	END
END Sing;
END BottlesOfBeer.
