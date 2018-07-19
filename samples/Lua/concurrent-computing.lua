co = {}
co[1] = coroutine.create( function() print "Enjoy" end )
co[2] = coroutine.create( function() print "Rosetta" end )
co[3] = coroutine.create( function() print "Code" end )

math.randomseed( os.time() )
h = {}
i = 0
repeat
    j = math.random(3)
    if h[j] == nil then
       coroutine.resume( co[j] )
       h[j] = true
       i = i + 1
    end
until i == 3
