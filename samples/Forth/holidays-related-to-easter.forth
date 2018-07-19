variable year

: ea_a year @ 19 mod ;
: ea_b year @ 100 / ;
: ea_c year @ 100 mod ;
: ea_d ea_b 4 / ;
: ea_e ea_b 4 mod ;
: ea_f ea_b 8 + 25 / ;
: ea_g ea_b ea_f - 1 + 3 / ;
: ea_h 19 ea_a * ea_b + ea_d - ea_g - 15 + 30 mod ;
: ea_i ea_c 4 / ;
: ea_k ea_c 4 mod ;
: ea_l 32 2 ea_e * + 2 ea_i * + ea_h - ea_k - 7 mod ;
: ea_m ea_a 11 ea_h * + 22 ea_i * + 451 / ;
: ea_numerator ea_h ea_l + 7 ea_m * - 114 + ;
: ea_month ea_numerator 31 / ;
: ea_day ea_numerator 31 mod 1 + ;
: bs ( -- backspace )
  8 emit ;

: monthname ( monthnum -- name )
  case
  1 of ." Jan" endof
  2 of ." Feb" endof
  3 of ." Mar" endof
  4 of ." Apr" endof
  5 of ." May" endof
  6 of ." Jun" endof
  7 of ." Jul" endof
  8 of ." Aug" endof
  9 of ." Sep" endof
  10 of ." Oct" endof
  11 of ." Nov" endof
  12 of ." Dec" endof
  dup . ." wrong input"
  endcase
;

: eastern ( year -- )
  dup year ! . bs ." :" space ea_day . bs space ea_month monthname ;

: main
  cr .\" year: eastern" cr 2200 400
  DO i eastern cr 100
  +LOOP
  cr 2021 2010
  DO i eastern cr
  LOOP
  ;
