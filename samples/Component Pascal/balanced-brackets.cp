MODULE Brackets;
IMPORT StdLog, Args, Stacks (* See Task Stacks *);
TYPE

	Character = POINTER TO RECORD (Stacks.Object)
		c: CHAR
	END;
	
PROCEDURE NewCharacter(c: CHAR): Character;
VAR
	n: Character;
BEGIN
	NEW(n);n.c:= c;RETURN n
END NewCharacter;

PROCEDURE (c: Character) Show*;
BEGIN
	StdLog.String("Character(");StdLog.Char(c.c);StdLog.String(");");StdLog.Ln
END Show;

PROCEDURE CheckBalance(str: ARRAY OF CHAR): BOOLEAN;
VAR
	s: Stacks.Stack;
	n,x: ANYPTR;
	i: INTEGER;
	c : CHAR;
BEGIN
	i := 0; s := Stacks.NewStack();
	WHILE (i < LEN(str$)) & (~Args.IsBlank(str[i])) & (str[i] # 0X) DO
		IF s.Empty() THEN
			s.Push(NewCharacter(str[i]));
		ELSE
			n := s.top.data;
			WITH
				n :  Character DO
				IF (str[i] = ']')& (n.c = '[') THEN
					x := s.Pop();
				ELSE
					s.Push(NewCharacter(str[i]))
				END;
			ELSE RETURN FALSE;
			END;
		END;
		INC(i)
	END;
	RETURN s.Empty();
END CheckBalance;

PROCEDURE Do*;
VAR
	p : Args.Params;
	i: INTEGER;
BEGIN
	Args.Get(p); (* Get Params *)
	FOR i := 0 TO p.argc - 1 DO
		StdLog.String(p.args[i] + ":>");StdLog.Bool(CheckBalance(p.args[i]));StdLog.Ln
	END
END Do;

END Brackets.
