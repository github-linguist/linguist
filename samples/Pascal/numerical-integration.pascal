function RectLeft(function f(x: real): real; xl, xr: real): real;
 begin
  RectLeft := f(xl)
 end;

function RectMid(function f(x: real): real; xl, xr: real) : real;
 begin
  RectMid := f((xl+xr)/2)
 end;

function RectRight(function f(x: real): real; xl, xr: real): real;
 begin
  RectRight := f(xr)
 end;

function Trapezium(function f(x: real): real; xl, xr: real): real;
 begin
  Trapezium := (f(xl) + f(xr))/2
 end;

function Simpson(function f(x: real): real; xl, xr: real): real;
 begin
  Simpson := (f(xl) + 4*f((xl+xr)/2) + f(xr))/6
 end;

function integrate(function method(function f(x: real): real; xl, xr: real): real;
                   function f(x: real): real;
                   a, b: real;
                   n: integer);
 var
  integral, h: real;
  k: integer;
 begin
  integral := 0;
  h := (b-a)/n;
  for k := 0 to n-1 do
   begin
    integral := integral + method(f, a + k*h, a + (k+1)*h)
   end;
  integrate := integral
 end;
