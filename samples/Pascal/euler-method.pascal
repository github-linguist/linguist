{$mode delphi}
PROGRAM Euler;

TYPE TNewtonCooling = FUNCTION (t: REAL) : REAL;

CONST	T0	: REAL = 100.0;
CONST	TR	: REAL = 20.0;
CONST	k 	: REAL = 0.07;
CONST	time    : INTEGER = 100; 	
CONST	step    : INTEGER = 10;
CONST	dt	: ARRAY[0..3] of REAL = (1.0,2.0,5.0,10.0);

VAR	i	: INTEGER;

FUNCTION NewtonCooling(t: REAL) : REAL;
	BEGIN
		NewtonCooling := -k * (t-TR);
	END;
	
PROCEDURE Euler(F: TNewtonCooling; y, h : REAL; n: INTEGER);
	VAR i: INTEGER = 0;
	BEGIN
		WRITE('dt=',trunc(h):2,':');
		REPEAT
			IF (i mod 10 = 0) THEN WRITE(' ',y:2:3);
			INC(i,trunc(h));
			y := y + h * F(y);	
		UNTIL (i >= n);
		WRITELN;
	END;

PROCEDURE Sigma;
	VAR t: INTEGER = 0;
	BEGIN
		WRITE('Sigma:');
		REPEAT
			WRITE(' ',(20 + 80 * exp(-0.07 * t)):2:3);
			INC(t,step);
		UNTIL (t>=time);
		WRITELN;
	END;

BEGIN
	WRITELN('Newton cooling function: Analytic solution (Sigma) with 3 Euler approximations.');
	WRITELN('Time: ',0:7,10:7,20:7,30:7,40:7,50:7,60:7,70:7,80:7,90:7);
	Sigma;
	FOR i := 1 to 3 DO
		Euler(NewtonCooling,T0,dt[i],time);
END.
