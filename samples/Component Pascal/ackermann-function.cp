MODULE NpctAckerman;

IMPORT  StdLog;

VAR
	m,n: INTEGER;

PROCEDURE Ackerman (x,y: INTEGER):INTEGER;

BEGIN
  IF    x = 0  THEN  RETURN  y + 1
  ELSIF y = 0  THEN  RETURN  Ackerman (x - 1 , 1)
  ELSE
    RETURN  Ackerman (x - 1 , Ackerman (x , y - 1))
  END
END Ackerman;

PROCEDURE Do*;
BEGIN
  FOR  m := 0  TO  3  DO
    FOR  n := 0  TO  6  DO
      StdLog.Int (Ackerman (m, n));StdLog.Char (' ')
    END;
    StdLog.Ln
  END;
  StdLog.Ln
END Do;

END NpctAckerman.
