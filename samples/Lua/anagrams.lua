-- Build the word set
local set = {}
local file = io.open("unixdict.txt")
local str = file:read()
while str do
    table.insert(set,str)
    str = file:read()
end

-- Build the anagram tree
local tree = {}
for i,word in next,set do
    -- Sort a string from lowest char to highest
    local function sortString(str)
        if #str <= 1 then
            return str
        end
        local less = ''
        local greater = ''
        local pivot = str:byte(1)
        for i = 2, #str do
            if str:byte(i) <= pivot then
                less = less..(str:sub(i,i))
            else
                greater = greater..(str:sub(i,i))
            end
        end
        return sortString(less)..str:sub(1,1)..sortString(greater)
    end
    local sortchar = sortString(word)
    if not tree[#word] then tree[#word] = {} end
    local node = tree[#word]
    for i = 1,#word do
        if not node[sortchar:byte(i)] then
            node[sortchar:byte(i)] = {}
        end
        node = node[sortchar:byte(i)]
    end
    table.insert(node,word)
end

-- Gather largest groups by gathering all groups of current max size and droping gathered groups and increasing max when a new largest group is found
local max = 0
local set = {}
local function recurse (tree)
    local num = 0
    for i,node in next,tree do
        if type(node) == 'string' then
            num = num + 1
        end
    end
    if num > max then
        set = {}
        max = num
    end
    if num == max then
        local newset = {}
        for i,node in next,tree do
            if type(node) == 'string' then
                table.insert(newset,node)
            end
        end
        table.insert(set,newset)
    end
    for i,node in next,tree do
        if type(node) == 'table' then
            recurse(node)
        end
    end
end

recurse (tree)
for i,v in next,set do io.write (i..':\t')for j,u in next,v do io.write (u..' ') end print() end
