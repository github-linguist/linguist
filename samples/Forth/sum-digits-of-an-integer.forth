: sum_int 0 begin over while swap base @ /mod swap rot + repeat nip ;

 2 base ! 11110 sum_int decimal  . cr
10 base ! 12345 sum_int decimal  . cr
16 base ! f0e   sum_int decimal  . cr
