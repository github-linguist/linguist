# GAP has an improved floating-point support since version 4.5

Pi := Acos(-1.0);

r := Pi / 5.0;
d := 36;

Deg := x -> x * Pi / 180;

Sin(r);         Asin(last);
Sin(Deg(d));    Asin(last);
Cos(r);         Acos(last);
Cos(Deg(d));    Acos(last);
Tan(r);         Atan(last);
Tan(Deg(d));    Atan(last);
