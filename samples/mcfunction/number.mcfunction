# test valid number
execute if score @a temp matches 1 run
say 123
say 1.0
say 0.1
say 123.456
say .1
say .789

# test valid number in selectors
execute as @a[distance=1] run
execute as @a[distance=123] run
execute as @a[distance=1.0] run
execute as @a[distance=0.1] run
execute as @a[distance=123.456] run
execute as @a[distance=.1] run
execute as @a[distance=.789] run

# test valid negative numbers
execute if score @a temp matches -1 run

# test invalid number
say .
say 1.

# test invalid number in selectors
execute as @a[distance=.] run
execute as @a[distance=1.] run
