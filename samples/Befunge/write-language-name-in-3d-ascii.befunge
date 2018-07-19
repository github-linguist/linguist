v THE DATA IMAGE.(totally useless after loading into stack)
>89                                   v Made by gamemanj
v0000122010000000000022201220000020000< For rosettacode
>9                                    v The 'image' is upsidedown
v2220100010002220101000101000000000000< The 3d offset isn't applied on this
>9                                    v and it's encoded weirdly
v1010122012001010101022201220000010000< Is that enough decoding needed
>9                                    v for the '3d' ascii requirement?
v2220100010001010222010101000000010000< Huh.(sees Batch) I think so.
>9                                    v 0:blank
v1000122012200000000022201220000010000< 1:\
>9                                    v 2:-
v                                 p000< 9:newline
>:9-v Check for 9:Newline               8:end
   v_v      (note that cell 0,0 isn't ever used after the first tick!)
 no   yes   |    |   >" ",v
     >$55+,00g1+:00p:|: -1< Newline With Spacing Encoder
^  v#               $<
   :
   8 check for end
 no-yes
  v_v
    >< make the program pause for the user forever.Ik,it's stupid

    Numeric Decoder \/
  >:1-v
     v_vCheck for 1 '\'
   NO: "YES
     2 \
     - "
    v_  >v
    "    "
^        -
    "    "
^ $,<  < < (code path reuse here,all 3 end in ,$ so I merged them)
