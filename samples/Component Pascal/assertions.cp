MODULE Assertions;
VAR
	x: INTEGER;
PROCEDURE DoIt*;
BEGIN
	x := 41;
	ASSERT(x = 42);
END DoIt;
END Assertions.

Assertions.DoIt
