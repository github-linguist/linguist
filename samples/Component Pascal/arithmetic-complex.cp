MODULE Complex;
IMPORT StdLog;
TYPE
        Complex* = POINTER TO ComplexDesc;
        ComplexDesc = RECORD
                r-,i-: REAL;
        END;

VAR
        r,x,y: Complex;

PROCEDURE New(x,y: REAL): Complex;
VAR
        r: Complex;
BEGIN
        NEW(r);r.r := x;r.i := y;
        RETURN r
END New;

PROCEDURE (x: Complex) Add*(y: Complex): Complex,NEW;
BEGIN
        RETURN New(x.r + y.r,x.i + y.i)
END Add;

PROCEDURE ( x: Complex) Sub*( y: Complex): Complex, NEW;
BEGIN
        RETURN New(x.r - y.r,x.i - y.i)
END Sub;

PROCEDURE ( x: Complex) Mul*( y: Complex): Complex, NEW;
BEGIN
        RETURN New(x.r*y.r - x.i*y.i,x.r*y.i + x.i*y.r)
END Mul;

PROCEDURE ( x: Complex) Div*( y: Complex): Complex, NEW;
VAR
        d: REAL;
BEGIN
        d := y.r * y.r + y.i * y.i;
        RETURN New((x.r*y.r + x.i*y.i)/d,(x.i*y.r - x.r*y.i)/d)
END Div;

(* Reciprocal *)
PROCEDURE (x: Complex) Rec*(): Complex,NEW;
VAR
        d: REAL;
BEGIN
        d := x.r * x.r + x.i * x.i;
        RETURN New(x.r/d,(-1.0 * x.i)/d);
END Rec;

(* Conjugate *)
PROCEDURE (x: Complex) Con*(): Complex,NEW;
BEGIN
        RETURN New(x.r, (-1.0) * x.i);
END Con;

PROCEDURE (x: Complex) Out(),NEW;
BEGIN
	   StdLog.String("Complex(");
	   StdLog.Real(x.r);StdLog.String(',');StdLog.Real(x.i);
	   StdLog.String("i );")
END Out;

PROCEDURE Do*;
BEGIN
        x := New(1.5,3);
        y := New(1.0,1.0);

        StdLog.String("x: ");x.Out();StdLog.Ln;
        StdLog.String("y: ");y.Out();StdLog.Ln;
        r := x.Add(y);
        StdLog.String("x + y: ");r.Out();StdLog.Ln;
        r := x.Sub(y);
        StdLog.String("x - y: ");r.Out();StdLog.Ln;
        r := x.Mul(y);
        StdLog.String("x * y: ");r.Out();StdLog.Ln;
        r := x.Div(y);
        StdLog.String("x / y: ");r.Out();StdLog.Ln;
        r := y.Rec();
        StdLog.String("1 / y: ");r.Out();StdLog.Ln;
        r := x.Con();
        StdLog.String("x': ");r.Out();StdLog.Ln;
END Do;

END Complex.
