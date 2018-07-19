values  = { 7, 6, 5, 4, 3, 2, 1, 0 }
indices = { 6, 1, 7 }

i = 1						-- discard duplicates
while i < #indices do
    j = i + 1
    while j < #indices do
	if indices[i] == indices[j] then
  	    table.remove( indices[j] )
	end
	j = j + 1
    end
    i = i + 1
end

for i = 1, #indices do
    indices[i] = indices[i] + 1      -- the tables of lua are one-based
end

vals = {}
for i = 1, #indices do
    vals[i] = values[ indices[i] ]
end

table.sort( vals )
table.sort( indices )

for i = 1, #indices do
    values[ indices[i] ] = vals[i]
end

for i = 1, #values do
    io.write( values[i], "  " )
end
