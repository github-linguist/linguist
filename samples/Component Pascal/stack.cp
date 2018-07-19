MODULE Stacks;
IMPORT StdLog;

TYPE
	(* some pointers to records *)
	Object* = POINTER TO ABSTRACT RECORD END;
	
	Integer = POINTER TO RECORD (Object)
		i: INTEGER
	END;
	
	Point = POINTER TO RECORD (Object)
		x,y: REAL
	END;

	Node = POINTER TO LIMITED RECORD
		next- : Node;
		data-: ANYPTR;
	END;

	(* Stack *)
	Stack* = POINTER TO RECORD
		top- : Node;
	END;
	
	PROCEDURE (dn: Object) Show*, NEW, ABSTRACT;
	
	PROCEDURE (i: Integer) Show*;
	BEGIN
		StdLog.String("Integer(");StdLog.Int(i.i);StdLog.String(");");StdLog.Ln
	END Show;
	
	PROCEDURE (p: Point) Show*;
	BEGIN
		StdLog.String("Point(");StdLog.Real(p.x);StdLog.Char(',');
		StdLog.Real(p.y);StdLog.String(");");StdLog.Ln
	END Show;
	
	PROCEDURE (s: Stack) Init, NEW;
	BEGIN
		s.top := NIL;
	END Init;
	
	PROCEDURE (s: Stack) Push*(data: ANYPTR), NEW;
	VAR
		n: Node;
	BEGIN
		NEW(n);n.next := NIL;n.data := data;
		IF s.top = NIL THEN
			s.top := n
		ELSE
			n.next := s.top;
			s.top := n
		END
	END Push;
	
	PROCEDURE (s: Stack) Pop*(): ANYPTR, NEW;
	VAR
		x: ANYPTR;
	BEGIN
		IF s.top # NIL THEN
			x := s.top.data;
			s.top := s.top.next
		ELSE
			x := NIL
		END;
		RETURN x
	END Pop;
	
	PROCEDURE (s: Stack) Empty*(): BOOLEAN, NEW;
	BEGIN
		RETURN s.top = NIL
	END Empty;
	
	PROCEDURE NewStack*(): Stack;
	VAR
		s: Stack;
	BEGIN
		NEW(s);s.Init;
		RETURN s
	END NewStack;
	
	PROCEDURE NewInteger*(data: INTEGER): Integer;
	VAR
		i: Integer;
	BEGIN
		NEW(i);i.i := data;
		RETURN i
	END NewInteger;
	
	PROCEDURE NewPoint*(x,y: REAL): Point;
	VAR
		p: Point;
	BEGIN
		NEW(p);p.x := x;p.y := y;
		RETURN p
	END NewPoint;
	
	PROCEDURE TestStack*;
	VAR
		s: Stack;
	BEGIN
		s := NewStack();
		s.Push(NewInteger(1));
		s.Push(NewPoint(2.0,3.4));
		s.Pop()(Object).Show();
		s.Pop()(Object).Show();
	END TestStack;
	
END Stacks.
