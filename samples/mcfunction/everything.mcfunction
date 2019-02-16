## Raycasting
#
# Casts a ray from starting position along a configurable number
# of blocks with a confugrable accuracy, counting the number of
# entities hit by the ray along the way.
#
# @params
#   mypack.raycast.distance temp
#       The number of blocks to cast forward.
#   mypack.raycast.precision temp
#       The ratio of block precision to a full block.
# @returns The number of entities hit by the ray.

function #mypack:hooks/raycast/begin

# TODO optimize entity selectors
function mypack:raycast/loop

function #mypack:hooks/raycast/end

# this is a comment
say hello world

    # indented comment
    say hello indent

effect give @s minecraft:night_vision 999999 1 true
effect give @s minecraft:night_vision 999999 1 false

teleport 1 2 3
teleport 100.5 80 -100.5

execute if score @a temp matches 10.. run
execute if score @a temp matches ..20 run
execute if score @a temp matches 10..20 run

execute positioned 10 ~ -10 run
execute positioned 10 ~10 -10 run
execute positioned 10 ~0.5 -10 run
execute positioned 10 ~.5 -10 run
execute positioned 10 ~-10 -10 run
execute positioned 10 ~-0.5 -10 run
execute positioned 10 ~-.5 -10 run

execute positioned 10 ^ -10 run
execute positioned 10 ^10 -10 run
execute positioned 10 ^0.5 -10 run
execute positioned 10 ^.5 -10 run
execute positioned 10 ^-10 -10 run
execute positioned 10 ^-0.5 -10 run
execute positioned 10 ^-.5 -10 run

function mypack:foo
function mypack:foo/bar
function mypack:foo/bar/baz
function #mypack:foo
function #mypack:foo/bar
function #mypack:foo/bar/baz

execute if block ~ ~ ~ minecraft:oak_log[axis=x] run
execute if block ~ ~ ~ minecraft:oak_leaves[distance=5] run
execute if block ~ ~ ~ minecraft:oak_leaves[persistent=true] run
execute if block ~ ~ ~ minecraft:oak_leaves[persistent=false] run
execute if block ~ ~ ~ minecraft:oak_leaves[distance=5,persistent=true] run
execute if block ~ ~ ~ #minecraft:leaves[distance=5] run
execute if block ~ ~ ~ #minecraft:leaves[distance=5,persistent=true] run

tag @s add my.tag

datapack enable "hello world"
datapack enable "escape \" me"

execute as f7a39418-72ca-4bf2-bc7e-ba9df67a4707 run
execute as 0-0-0-0-0 run

execute store result score #fakeplayer
execute store result score #fake.player
execute store result score #fake_player
execute store result score $fakeplayer
execute store result score %fakeplayer

execute as @b
execute as @a

execute as @a[tag=foo]
execute as @a[tag=!foo]

execute as @a[sort=nearest] run
execute as @a[gamemode=survival] run
execute as @a[gamemode=!creative] run
execute as @a[tag=foo,tag=bar,tag=!baz] run

execute as @a[distance=15]
execute as @a[distance=1.5]
execute as @a[distance=.5]
execute as @a[distance=-.25]

execute as @a[distance=100]
execute as @a[distance=..10]
execute as @a[distance=11..19]
execute as @a[distance=20..]
execute as @a[distance=0.5]
execute as @a[distance=..0.1]
execute as @a[distance=0.2..0.8]
execute as @a[distance=0.9..]

execute as @a[type=minecraft:bat] run
execute as @a[type=!minecraft:cow,type=!minecraft:pig] run
execute as @a[type=#minecraft:skeletons] run
execute as @a[type=!#minecraft:skeletons,type=!minecraft:zombie] run

execute as @a[tag=my.tag] run

execute as @a[name="hello world"] as @s run
execute as @a[name="escape \" me"] as @s run
execute as @a[name="how, about, commas ?"] as @s run
execute as @a[name="and [braces] ?"] as @s run

execute as @e[nbt={ PortalCooldown: 0 }] run
execute as @e[nbt={ Item: {id: "minecraft:diamond", Count: 64 } }] run

execute if score @s foo < @s bar run
execute if score @s foo <= @s bar run
execute if score @s foo = @s bar run
execute if score @s foo > @s bar run
execute if score @s foo >= @s bar run

data get entity @s SelectedItem.tag.display.Name
data get entity @s Inventory[0]
data get entity @s Inventory[{id: "minecraft:diamond"}].Count
data get entity @s Inventory[].tag{custom: true}.display.Name

data merge entity @s { foo: true, bar: 1234 }
data modify block ~ ~ ~ RecordItem.tag set value { messages: [hi, bye] }
data modify block ~ ~ ~ RecordItem.tag.messages append value [ { message: "hello world" } ]

tellraw @a {"text": "hello world", "color": "blue"}
tellraw @a [{"text": "hello", "color": "blue"}, {"text": "world", "color": "blue"}]
