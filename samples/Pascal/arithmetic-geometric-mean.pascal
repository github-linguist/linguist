Program ArithmeticGeometricMean;

uses
  gmp;

procedure agm (in1, in2: mpf_t; var out1, out2: mpf_t);
begin
  mpf_add (out1, in1, in2);
  mpf_div_ui (out1, out1, 2);
  mpf_mul (out2, in1, in2);
  mpf_sqrt (out2, out2);
end;

const
  nl = chr(13)+chr(10);
var
  x0, y0, resA, resB: mpf_t;
  i: integer;
begin
  mpf_set_default_prec (65568);

  mpf_init_set_ui (y0, 1);
  mpf_init_set_d (x0, 0.5);
  mpf_sqrt (x0, x0);
  mpf_init (resA);
  mpf_init (resB);

  for i := 0 to 6 do
  begin
    agm(x0, y0, resA, resB);
    agm(resA, resB, x0, y0);
  end;
  mp_printf ('%.20000Ff'+nl, @x0);
  mp_printf ('%.20000Ff'+nl+nl, @y0);
end.
