N = 2

lst = { { "Tyler Bennett","E10297",32000,"D101" },
	{ "John Rappl","E21437",47000,"D050" },
	{ "George Woltman","E00127",53500,"D101" },
	{ "Adam Smith","E63535",18000,"D202" },
	{ "Claire Buckman","E39876",27800,"D202" },
	{ "David McClellan","E04242",41500,"D101" },
	{ "Rich Holcomb","E01234",49500,"D202" },
	{ "Nathan Adams","E41298",21900,"D050" },
	{ "Richard Potter","E43128",15900,"D101" },
	{ "David Motsinger","E27002",19250,"D202" },
	{ "Tim Sampair","E03033",27000,"D101" },
	{ "Kim Arlich","E10001",57000,"D190" },
	{ "Timothy Grove","E16398",29900,"D190" }
      }

dep = {}
for i = 1, #lst do
    if dep[ lst[i][4] ] == nil then
	dep[ lst[i][4] ] = {}
	dep[ lst[i][4] ][1] = lst[i]
    else
	dep[ lst[i][4] ][#dep[lst[i][4]]+1] = lst[i]
    end
end

for i, _ in pairs( dep ) do
    table.sort( dep[i], function (a,b) return a[3] > b[3] end )
	
    print( "Department:", dep[i][1][4] )
    for l = 1, math.min( N, #dep[i] ) do
	print( "", dep[i][l][1], dep[i][l][2], dep[i][l][3] )
    end
    print ""
end
