: quaternions  4 * floats ;

: qvariable create 1 quaternions allot ;

: q! ( a b c d q -- )
  dup 3 floats + f!  dup 2 floats + f!  dup float+ f!  f! ;

: qcopy ( src dest -- ) 1 quaternions move ;

: qnorm  ( q -- f )
  0e 4 0 do  dup f@ fdup f* f+  float+ loop drop fsqrt ;

: qf* ( q f -- )
  4 0 do dup f@ fover f* dup f!  float+ loop fdrop drop ;

: qnegate ( q -- )  -1e qf* ;

: qconj ( q -- )
  float+ 3 0 do dup f@ fnegate dup f!  float+ loop drop ;

: qf+ ( q f -- ) dup f@ f+ f! ;

: q+ ( q1 q2 -- )
  4 0 do over f@ dup f@ f+ dup f!  float+ swap float+ swap loop 2drop ;

\ access
: q.a             f@ ;
: q.b      float+ f@ ;
: q.c  2 floats + f@ ;
: q.d  3 floats + f@ ;

: q* ( dest q1 q2 -- )
  over q.a dup q.d f*  over q.b dup q.c f* f+  over q.c dup q.b f* f-  over q.d dup q.a f* f+
  over q.a dup q.c f*  over q.b dup q.d f* f-  over q.c dup q.a f* f+  over q.d dup q.b f* f+
  over q.a dup q.b f*  over q.b dup q.a f* f+  over q.c dup q.d f* f+  over q.d dup q.c f* f-
  over q.a dup q.a f*  over q.b dup q.b f* f-  over q.c dup q.c f* f-  over q.d dup q.d f* f-
  2drop  4 0 do dup f!  float+ loop  drop ;

: q= ( q1 q2 -- ? )
  4 0 do
    over f@ dup f@ f<> if 2drop false unloop exit then
    float+ swap float+
  loop
  2drop true ;

\ testing

: q. ( q -- )
  [char] ( emit space
  4 0 do dup f@ f.  float+ loop drop
  [char] ) emit space ;

qvariable q   1e 2e 3e 4e q  q!
qvariable q1  2e 3e 4e 5e q1 q!
create q2     3e f, 4e f, 5e f, 6e f,	\ by hand

qvariable tmp
qvariable m1
qvariable m2

q qnorm f.				\ 5.47722557505166
q tmp qcopy  tmp qnegate  tmp q.	\ ( -1. -2. -3. -4. )
q tmp qcopy  tmp qconj    tmp q.	\ ( 1. -2. -3. -4. )

q m1 qcopy  m1 7e qf+   m1 q.		\ ( 8. 2. 3. 4. )
q m2 qcopy  7e m2 qf+   m2 q.		\ ( 8. 2. 3. 4. )
m1 m2 q= .				\ -1  (true)

q2 tmp qcopy  q1 tmp q+   tmp q.	\ ( 5. 7. 9. 11. )

q m1 qcopy  m1 7e qf*     m1 q.		\ ( 7. 14. 21. 28. )
q m2 qcopy  7e m2 qf*     m2 q.		\ ( 7. 14. 21. 28. )
m1 m2 q= .				\ -1  (true)

m1 q1 q2 q*  m1 q.			\ ( -56. 16. 24. 26. )
m2 q2 q1 q*  m2 q.			\ ( -56. 18. 20. 28. )
m1 m2 q= .				\ 0  (false)
