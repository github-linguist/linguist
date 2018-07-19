function mean (numlist)
    if type(numlist) ~= 'table' then return numlist end
    num = 0
    table.foreach(numlist,function(i,v) num=num+v end)
    return num / #numlist
end

print (mean({3,1,4,1,5,9}))
