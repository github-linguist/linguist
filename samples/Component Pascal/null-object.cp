MODULE ObjectNil;
IMPORT StdLog;
TYPE
	Object = POINTER TO ObjectDesc;
	ObjectDesc = RECORD
	END;
VAR	
	x: Object; (* default initialization to NIL *)
	
PROCEDURE DoIt*;
BEGIN
	IF x = NIL THEN
		StdLog.String("x is NIL");StdLog.Ln
	END
END DoIt;

END ObjectNil.
