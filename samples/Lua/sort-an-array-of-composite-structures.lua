function sorting( a, b )
    return a[1] < b[1]
end

tab = { {"C++", 1979}, {"Ada", 1983}, {"Ruby", 1995}, {"Eiffel", 1985} }

table.sort( tab, sorting )
for _, v in ipairs( tab ) do
    print( unpack(v) )
end
