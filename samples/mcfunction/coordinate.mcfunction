# test valid relative coordinates
execute positioned 10 ~ -10 run
execute positioned 10 ~10 -10 run
execute positioned 10 ~0.5 -10 run
execute positioned 10 ~.5 -10 run
execute positioned 10 ~-10 -10 run
execute positioned 10 ~-0.5 -10 run
execute positioned 10 ~-.5 -10 run

# test valid local coordinates
execute positioned 10 ^ -10 run
execute positioned 10 ^10 -10 run
execute positioned 10 ^0.5 -10 run
execute positioned 10 ^.5 -10 run
execute positioned 10 ^-10 -10 run
execute positioned 10 ^-0.5 -10 run
execute positioned 10 ^-.5 -10 run

# test invalid relative coordinates
execute positioned 10 ~. -10 run
execute positioned 10 ~5. -10 run
execute positioned 10 ~- -10 run
execute positioned 10 ~-. -10 run
execute positioned 10 ~-5. -10 run

# test invalid local coordinates
execute positioned 10 ^. -10 run
execute positioned 10 ^5. -10 run
execute positioned 10 ^- -10 run
execute positioned 10 ^-. -10 run
execute positioned 10 ^-5. -10 run
