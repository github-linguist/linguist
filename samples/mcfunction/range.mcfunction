# test valid range with integers
execute if score @a temp matches 10.. run
execute if score @a temp matches ..20 run
execute if score @a temp matches 10..20 run

# test valid range with integers in selectors
execute as @a[distance=10..] run
execute as @a[distance=..20] run
execute as @a[distance=11..19] run

# test valid range with negative integers
execute if score @a temp matches -1.. run
execute if score @a temp matches ..-1 run
execute if score @a temp matches -2..-1 run
execute if score @a temp matches -1..1 run

# test valid range with negative integers in selectors
execute as @a[x_rotation=-1..] run
execute as @a[x_rotation=..-1] run
execute as @a[x_rotation=-2..-1] run
execute as @a[x_rotation=-1..1] run

# test valid range with decimals in selectors
execute as @a[distance=0.1..] run
execute as @a[distance=0.2..0.8] run
execute as @a[distance=..0.9] run
execute as @a[distance=.1..] run
execute as @a[distance=.2..0.8] run
execute as @a[distance=0.2...8] run
execute as @a[distance=.2...8] run
execute as @a[distance=...9] run

# test valid range with negative decimals in selectors
execute as @a[x_rotation=-0.1..] run
execute as @a[x_rotation=-.1..] run
execute as @a[x_rotation=..-0.1] run
execute as @a[x_rotation=..-.1] run
execute as @a[x_rotation=-0.2..-0.1] run
execute as @a[x_rotation=-.2..-.1] run
execute as @a[x_rotation=-0.1..0.1] run
execute as @a[x_rotation=-.1...1] run

# test valid mixed range in selectors
execute as @a[x_rotation=0..0.2] run
execute as @a[x_rotation=0...2] run
execute as @a[distance=0.2..1] run
execute as @a[distance=.2..1] run

# test invalid range
execute if score @a temp matches .. run

# test invalid range in selectors
execute as @a[distance=..] run
