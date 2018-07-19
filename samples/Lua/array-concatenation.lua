a = {1,2,3}
b = {4,5,6}
table.foreach(b,function(i,v)table.insert(a,v)end)
for i,v in next,a do io.write (v..' ') end
