# test scoreboard players operation
scoreboard players operation @s foo %= @s bar
scoreboard players operation @s foo *= @s bar
scoreboard players operation @s foo += @s bar
scoreboard players operation @s foo -= @s bar
scoreboard players operation @s foo /= @s bar
scoreboard players operation @s foo < @s bar
scoreboard players operation @s foo = @s bar
scoreboard players operation @s foo > @s bar
scoreboard players operation @s foo >< @s bar

# test execute if score
execute if score @s foo < @s bar run
execute if score @s foo <= @s bar run
execute if score @s foo = @s bar run
execute if score @s foo > @s bar run
execute if score @s foo >= @s bar run
