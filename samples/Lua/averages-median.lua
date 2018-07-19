function median (numlist)
    if type(numlist) ~= 'table' then return numlist end
    table.sort(numlist)
    if #numlist %2 == 0 then return (numlist[#numlist/2] + numlist[#numlist/2+1]) / 2 end
    return numlist[math.ceil(#numlist/2)]
end

print(median({4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2}))
print(median({4.1, 7.2, 1.7, 9.3, 4.4, 3.2}))
