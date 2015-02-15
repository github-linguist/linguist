MODULE LeapYear;
IMPORT StdLog, Strings, Args;

PROCEDURE IsLeapYear(year: INTEGER): BOOLEAN;
BEGIN
	IF year MOD 4 # 0 THEN
    	RETURN FALSE
	ELSE
		IF year MOD 100 = 0 THEN
			IF year MOD 400  = 0 THEN RETURN TRUE ELSE RETURN FALSE END
		ELSE
			RETURN TRUE
		END
	END
END IsLeapYear;

PROCEDURE Do*;
VAR
	p: Args.Params;
	year,done,i: INTEGER;
BEGIN
	Args.Get(p);
	FOR i := 0 TO p.argc - 1 DO
		Strings.StringToInt(p.args[i],year,done);
		StdLog.Int(year);StdLog.String(":>");StdLog.Bool(IsLeapYear(year));StdLog.Ln
	END;
	
END Do;
END LeapYear.
