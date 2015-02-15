   err =. 2^ 53-~ 2 <.@^. |   NB. get the size of one-half unit in the last place
   safeadd =. + (-,+) +&err
   0j15": 1.14 safeadd 2000.0 NB. print with 15 digits after the decimal
2001.139999999999873 2001.140000000000327
