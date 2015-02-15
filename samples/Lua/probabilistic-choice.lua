items = {}
items["aleph"]  = 1/5.0
items["beth"]   = 1/6.0
items["gimel"]  = 1/7.0
items["daleth"] = 1/8.0
items["he"]     = 1/9.0
items["waw"]    = 1/10.0
items["zayin"]  = 1/11.0
items["heth"]   = 1759/27720

num_trials = 1000000

samples = {}
for item, _ in pairs( items ) do
    samples[item] = 0
end

math.randomseed( os.time() )
for i = 1, num_trials do
    z = math.random()
	
    for item, _ in pairs( items ) do
	if z < items[item] then
	    samples[item] = samples[item] + 1
	    break;
	else
 	    z = z - items[item]	
	end
    end
end
	
for item, _ in pairs( items ) do
    print( item, samples[item]/num_trials, items[item] )
end
