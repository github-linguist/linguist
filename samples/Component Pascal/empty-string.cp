MODULE EmptyString;
IMPORT StdLog;

PROCEDURE Do*;
VAR
	s: ARRAY 64 OF CHAR;
	(* s := "" <=> s[0] := 0X => s isEmpty*)
BEGIN
	s := "";
	StdLog.String("Is 's' empty?:>  ");StdLog.Bool(s = "");StdLog.Ln;
	StdLog.String("Is not 's' empty?:> ");StdLog.Bool(s # "");StdLog.Ln;
	StdLog.Ln;
	(* Or *)
	s := 0X;
	StdLog.String("Is 's' empty?:>  ");StdLog.Bool(s = 0X);StdLog.Ln;
	StdLog.String("Is not 's' empty?:> ");StdLog.Bool(s # 0X);StdLog.Ln;
	StdLog.Ln;	
END Do;
END EmptyString.
