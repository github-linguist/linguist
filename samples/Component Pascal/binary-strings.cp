MODULE NpctBinaryString;
IMPORT StdLog,Strings;

PROCEDURE Do*;
VAR
	str: ARRAY 256 OF CHAR;
	pStr,pAux: POINTER TO ARRAY OF CHAR;
	b: BYTE;
	pIni: INTEGER;
BEGIN
	(* String creation, on heap *)
	NEW(pStr,256); (* Garbage collectable *)
	NEW(pAux,256);
	
	(* String assingment *)
	pStr^ := "This is a string on a heap";
	pAux^ := "This is a string on a heap";
	str := "This is other string";
	
	(* String comparision *)
	StdLog.String("pStr = str:> ");StdLog.Bool(pStr$ = str$);StdLog.Ln;
	StdLog.String("pStr = pAux:> ");StdLog.Bool(pStr$ = pAux$);StdLog.Ln;
	
	(* String cloning and copying *)
	NEW(pAux,LEN(pStr$) + 1);pAux^ := pStr$;
	
	(* Check if a string is empty *)
	(* version 1 *)
	pAux^ := "";
	StdLog.String("is empty pAux?(1):> ");StdLog.Bool(pAux$ = "");StdLog.Ln;
	(* version 2 *)
	pAux[0] := 0X;
	StdLog.String("is empty pAux?(2):> ");StdLog.Bool(pAux$ = "");StdLog.Ln;
	(* version 3 *)
	pAux[0] := 0X;
	StdLog.String("is empty pAux?(3):> ");StdLog.Bool(pAux[0] = 0X);StdLog.Ln;
	(* version 4 *)
	pAux^ := "";
	StdLog.String("is empty pAux?(4):> ");StdLog.Bool(pAux[0] = 0X);StdLog.Ln;
	
	(* Append a byte to a string *)
	NEW(pAux,256);pAux^ := "BBBBBBBBBBBBBBBBBBBBB";
	b := 65;pAux[LEN(pAux$)] := CHR(b);
	StdLog.String("pAux:> ");StdLog.String(pAux);StdLog.Ln;
	
	(* Extract a substring from a string *)
	Strings.Extract(pStr,0,16,pAux);
	StdLog.String("pAux:> ");StdLog.String(pAux);StdLog.Ln;
	
	(* Replace a every ocurrence of a string with another string *)
	pAux^ := "a"; (* Pattern *)
	Strings.Find(pStr,pAux,0,pIni);
	WHILE pIni > 0 DO
		Strings.Replace(pStr,pIni,LEN(pAux$),"one");
		Strings.Find(pStr,pAux,pIni + 1,pIni);
	END;
	StdLog.String("pStr:> ");StdLog.String(pStr);StdLog.Ln;
	
	(* Join strings *)
	pStr^ := "First string";pAux^ := "Second String";
	str := pStr$ + "." + pAux$;
	StdLog.String("pStr + '.' + pAux:>");StdLog.String(str);StdLog.Ln
END Do;
END NpctBinaryString.
