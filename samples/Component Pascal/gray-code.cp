MODULE GrayCodes;
IMPORT StdLog,SYSTEM;

PROCEDURE Encode*(i: INTEGER; OUT x: INTEGER);
VAR
	j: INTEGER;
	s,r: SET;
BEGIN
	s := BITS(i);j := MAX(SET);
	WHILE (j >= 0) & ~(j IN s) DO DEC(j) END;
	r := {};IF j >= 0 THEN INCL(r,j) END;
	WHILE j > 0 DO
		IF ((j IN s) & ~(j - 1 IN s)) OR (~(j IN s) & (j - 1 IN s)) THEN INCL(r,j-1) END;
		DEC(j)
	END;
	x := SYSTEM.VAL(INTEGER,r)
END Encode;

PROCEDURE Decode*(x: INTEGER; OUT i: INTEGER);
VAR
	j: INTEGER;
	s,r: SET;
BEGIN
	s := BITS(x);r:={};j := MAX(SET);
	WHILE (j >= 0) & ~(j IN s) DO DEC(j) END;
	IF j >= 0 THEN INCL(r,j) END;
	WHILE j > 0 DO
		IF ((j IN r) & ~(j - 1 IN s)) OR (~(j IN r) & (j - 1 IN s)) THEN INCL(r,j-1) END;
		DEC(j)
	END;
	i := SYSTEM.VAL(INTEGER,r);
END Decode;


PROCEDURE Do*;
VAR
	grayCode,binCode: INTEGER;
	i: INTEGER;
BEGIN
	StdLog.String(" i ");StdLog.String("     bin code    ");StdLog.String("   gray code    ");StdLog.Ln;
	StdLog.String("---");StdLog.String(" ----------------");StdLog.String(" ---------------");StdLog.Ln;
	FOR i := 0 TO 32 DO;
		Encode(i,grayCode);Decode(grayCode,binCode);
		StdLog.IntForm(i,10,3,' ',FALSE);
		StdLog.IntForm(binCode,2,16,' ',TRUE);
		StdLog.IntForm(grayCode,2,16,' ',TRUE);
		StdLog.Ln;
	END
END Do;

END GrayCodes.
