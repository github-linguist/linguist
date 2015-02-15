MODULE Graphics;
IMPORT StdLog;
TYPE
	(* class *)
	Point* = POINTER TO LIMITED RECORD
		x-,y-: INTEGER; (* Instance variables *)
	END;
	
	(* method *)
	PROCEDURE (p: Point) Abs*(): INTEGER,NEW;
	BEGIN
		RETURN p.x
	END Abs;
	
	(* method *)
	PROCEDURE (p: Point) Ord*(): INTEGER,NEW;
	BEGIN
		RETURN p.y
	END Ord;
	
	(* method *)
	PROCEDURE (p: Point) Show*,NEW;
	BEGIN
		StdLog.String("Point(");StdLog.Int(p.x);StdLog.String(",");
		StdLog.Int(p.y);StdLog.String(");");StdLog.Ln
	END Show;
	
	(* constructor *)
	PROCEDURE NewPoint*(x,y: INTEGER): Point;
	VAR
		p: Point;
	BEGIN
		NEW(p);p.x := x;p.y := y;
		RETURN p
	END NewPoint;
	
	PROCEDURE TestPoint*;
	VAR
		p: Point;
	BEGIN
		p := NewPoint(10,20);
		p.Show();
		StdLog.String("Abs:> ");StdLog.Int(p.Abs());StdLog.Ln;
		StdLog.String("Ord:> ");StdLog.Int(p.Ord());StdLog.Ln
	END TestPoint;
END Graphics.
