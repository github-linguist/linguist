Program ModularExponentiation(output);

uses
  gmp;

var
  a, b, m, r: mpz_t;
  fmt: pchar;

begin
  mpz_init_set_str(a, '2988348162058574136915891421498819466320163312926952423791023078876139', 10);
  mpz_init_set_str(b, '2351399303373464486466122544523690094744975233415544072992656881240319', 10);
  mpz_init(m);
  mpz_ui_pow_ui(m, 10, 40);

  mpz_init(r);
  mpz_powm(r, a, b, m);

  fmt := '%Zd' + chr(13) + chr(10);
  mp_printf(fmt, @r); (* ...16808958343740453059 *)

  mpz_clear(a);
  mpz_clear(b);
  mpz_clear(m);
  mpz_clear(r);
end.
