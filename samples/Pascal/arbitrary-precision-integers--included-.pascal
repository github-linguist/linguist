program GMP_Demo;

uses
  math, gmp;

var
  a:   mpz_t;
  out: pchar;
  len: longint;
  i:   longint;

begin
  mpz_init_set_ui(a, 5);
  mpz_pow_ui(a, a, 4 ** (3 ** 2));
  len := mpz_sizeinbase(a, 10);
  writeln('GMP says size is: ', len);
  out := mpz_get_str(NIL, 10, a);
  writeln('Actual size is:   ', length(out));
  write('Digits: ');
  for i := 0 to 19 do
    write(out[i]);
  write ('...');
  for i := len - 20 to len do
    write(out[i]);
  writeln;
end.
