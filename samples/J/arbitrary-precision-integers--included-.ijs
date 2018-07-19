   Pow5432=: 5^4^3^2x
   Pow5432=: ^/ 5 4 3 2x                    NB. alternate J solution
   # ": Pow5432                             NB. number of digits
183231
   20 ({. , '...' , -@[ {. ]) ": Pow5432    NB. 20 first & 20 last digits
62060698786608744707...92256259918212890625
