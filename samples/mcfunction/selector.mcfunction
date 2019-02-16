# test valid
execute as @b
execute as @a
execute as @a[]
execute as @a[tag=foo]
execute as @a[tag=!foo]

# test valid with trailing command
execute as @a run
execute as @a[] run
execute as @a[tag=foo] run
execute as @a[tag=!foo] run

# test valid with mixed selectors
execute as @a as @a run
execute as @a as @a[sort=nearest] run
execute as @a[sort=nearest] as @a run
execute as @a[sort=nearest] as @a[sort=nearest] run
teleport @a @s

# test valid multiple arguments
execute as @a[sort=nearest,limit=1] run
execute as @a[sort=nearest,limit=1] run
execute as @a[sort=nearest,limit=1,distance=0] run
execute as @a[sort=nearest,tag=!foo,tag=bar] run

# test valid whitespace around arguments

execute as @a[sort=nearest] run
execute as @a[ sort=nearest] run
execute as @a[sort=nearest ] run
execute as @a[ sort=nearest ] run

execute as @a[sort=nearest,limit=1] run
execute as @a[ sort=nearest,limit=1] run
execute as @a[sort=nearest,limit=1 ] run
execute as @a[ sort=nearest,limit=1 ] run

execute as @a[sort=nearest, limit=1] run
execute as @a[ sort=nearest, limit=1] run
execute as @a[sort=nearest, limit=1 ] run
execute as @a[ sort=nearest, limit=1 ] run

execute as @a[sort=nearest ,limit=1] run
execute as @a[ sort=nearest ,limit=1] run
execute as @a[sort=nearest ,limit=1 ] run
execute as @a[ sort=nearest ,limit=1 ] run

execute as @a[sort=nearest , limit=1] run
execute as @a[ sort=nearest , limit=1] run
execute as @a[sort=nearest , limit=1 ] run
execute as @a[ sort=nearest , limit=1 ] run

execute as @a[ sort =nearest , limit=1 ] run
execute as @a[ sort= nearest , limit=1 ] run
execute as @a[ sort = nearest , limit=1 ] run

execute as @a[  tag  =  foo  ,  limit  =  1  ] run
execute as @a[  tag  =  !  foo  ,  limit  =  1  ] run

# test valid arguments in separate selectors
execute as @a[sort=nearest] as @s[tag=foo] run
execute as @a[sort=nearest,limit=1] as @a[tag=foo] run
execute as @a[sort=nearest] as @a[tag=foo,tag=bar] run
execute as @a[sort=nearest,limit=1] as @a[tag=foo,tag=bar] run

# test valid resource location argument
execute as @a[type=minecraft:bat] run
execute as @a[ type = minecraft:bat ] run
execute as @a[ type = ! minecraft:bat ] run
execute as @a[type=minecraft:bat,tag=foo] run
execute as @a[tag=foo,type=minecraft:bat] run
execute as @a[tag=foo,type=minecraft:bat,tag=foo] run

# test valid tagged resource location argument
execute as @a[type=#minecraft:skeletons] run
execute as @a[ type = #minecraft:skeletons ] run
execute as @a[ type = ! #minecraft:skeletons ] run
execute as @a[type=#minecraft:skeletons,tag=foo] run
execute as @a[tag=foo,type=#minecraft:skeletons] run
execute as @a[tag=foo,type=#minecraft:skeletons,tag=foo] run

# test valid range argument
execute as @a[distance=..10]
execute as @a[distance=11..19]
execute as @a[distance=20..]
execute as @a[distance=0.5]
execute as @a[distance=..0.1]
execute as @a[distance=0.2..0.8]
execute as @a[distance=0.9..]
execute as @a[tag=foo,distance=1..,tag=bar]
execute as @a[ tag = foo , distance = 1.. , tag = bar ]

# test valid number argument
execute as @a[distance=15]
execute as @a[distance=1.5]
execute as @a[distance=.5]
execute as @a[distance=-.25]
execute as @a[tag=foo,distance=-.25,tag=bar]
execute as @a[ tag = foo , distance = -.25 , tag = bar ]

# test valid boolean argument (were it to exist)
execute as @a[somekey=true]
execute as @a[somekey=false]
execute as @a[ somekey = true ]
execute as @a[tag=foo,somekey=true,tag=bar]
execute as @a[ tag = foo , somekey = true , tag = bar ]

# test valid literal argument
execute as @a[sort=nearest] run
execute as @a[tag=foo,sort=nearest,tag=bar] run
execute as @a[ tag = foo , sort = nearest , tag = bar ] run

# test valid unquoted string argument
execute as @a[tag=my.custom.tag] run
execute as @a[tag=foo,tag=my.custom.tag,tag=bar] run
execute as @a[ tag = foo , tag = my.custom.tag , tag = bar ] run

# test valid quoted string argument
execute as @a[tag=foo,name="[",tag=bar] as @s run
execute as @a[tag=foo,name="]",tag=bar] as @s run
execute as @a[tag=foo,name="[]",tag=bar] as @s run
execute as @a[tag=foo,name="[[]]",tag=bar] as @s run
execute as @a[tag=foo,name="foo, bar"] as @s run
execute as @a[tag=foo,name="escaped \" quote"] as @s run
execute as @a[  tag  =  foo  ,  name  =  "  lots of space  "  ,  tag  =  bar  ] as @s run

# test valid nbt argument
execute as @e[nbt={PortalCooldown:0}] run
execute as @e[nbt={ PortalCooldown : 0 }] run
execute as @e[ nbt = { PortalCooldown : 0 } ] run
execute as @e[nbt={Item:{id:"minecraft:diamond",Count:64}}] run
execute as @e[nbt={ Item : { id : "minecraft:diamond", Count : 64 } }] run
execute as @e[ nbt = { Item : { id : "minecraft:diamond" , Count : 64 } } ] run

# test valid scores argument
execute as @a[scores={myscore=10}] run
execute as @a[scores={myscore=10..12}] run
execute as @a[scores={myscore=5..}] run
execute as @a[scores={myscore=..15}] run
execute as @a[scores={foo=10,bar=1..5}] run
execute as @a[scores={foo=10,bar=1..5,baz=..0}] run
execute as @a[ scores = {foo=10,bar=1..5,baz=..0} ] run
execute as @a[ scores = { foo = 10 , bar = 1..5 , baz = ..0 } ] run

# test valid advancements argument
execute as @a[advancements={minecraft:story/form_obsidian=true}] run
execute as @a[advancements={minecraft:story/form_obsidian=false}] run
execute as @a[advancements={minecraft:story/obtain_armor={iron_helmet=true}}] run
execute as @a[advancements={minecraft:story/follow_ender_eye=true}] run
execute as @a[advancements={minecraft:story/form_obsidian=true,minecraft:story/follow_ender_eye=true}] run
execute as @a[advancements={minecraft:story/form_obsidian=true,minecraft:story/follow_ender_eye=true,minecraft:story/obtain_armor=true}] run
execute as @a[ advancements = {minecraft:story/follow_ender_eye=true} ] run
execute as @a[ advancements = { minecraft:story/follow_ender_eye = true } ] run
execute as @a[ advancements = { minecraft:story/form_obsidian = true , minecraft:story/follow_ender_eye = true , minecraft:story/obtain_armor = true } ] run

# test invalid edge cases
execute as @a[,] as @s run
execute as @a[,,] as @s run
execute as @a[,tag=foo] as @s run
execute as @a[tag=foo,] as @s run
execute as @a[,tag=foo,] as @s run
execute as @a[tag,=foo] as @s run
execute as @a[tag=,foo] as @s run
execute as @a[tag,=,foo] as @s run
execute as @a[,tag,=,foo,] as @s run
execute as @a[tag=foo,name="[[]]"],tag=bar] as @s run

# test invalid
execute as @
execute as @A
execute as @ab
execute as @0
execute as @_
execute as @a[
execute as @a]

# test invalid with trailing command
execute as @ run
execute as @a[ run
execute as @a] run

# test invalid basic arguments
execute as @a[sort] run
execute as @a[sort=] run
execute as @a[=nearest] run

# test invalid quoted string argument
execute as @a[name=my name] run
execute as @a[name="my name] run
execute as @a[name=my name"] run

# test invalid resource location argument
execute as @a[type=mypack:] run
execute as @a[type=:foo] run
execute as @a[type=#mypack:] run
execute as @a[type=#:foo] run

# test invalid edge cases
execute as @a[gamemode=survival,]] run
