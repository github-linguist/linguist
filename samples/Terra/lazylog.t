local function gentrees(depth)
    local function treetostring(tree)
        if type(tree) == "string" then return tree
        elseif tree.rhs then return "(" .. treetostring(tree.lhs) .. " " .. tree.op .. " " .. treetostring(tree.rhs) .. ")"
        else return "(" .. tree.op .. " " .. treetostring(tree.lhs) .. ")" end
    end
    local function gendepth(depth)
        local trees = {}
        table.insert(trees, "%s")
        if depth == 0 then return trees end
        local subtrees = gendepth(depth - 1)
        for _,t0 in ipairs(subtrees) do
            table.insert(trees, { lhs = t0, op = "not" })
            for _,t1 in ipairs(subtrees) do
                table.insert(trees, { lhs = t0, rhs = t1, op = "and" })
                table.insert(trees, { lhs = t0, rhs = t1, op = "or" })
            end
        end
        return trees
    end
    local trees = gendepth(depth)
    local nargs = 2 ^ depth
    local argassign = "local i0"
    local targs = "return terra(i0 : bool"
    local args = {"i0"}
    for i = 1, nargs - 1 do
        local arg = "i"..tostring(i)
        table.insert(args,arg)
        argassign = argassign .. ", " .. arg
        targs = targs .. ", " .. arg .. " : bool"
    end
    argassign = argassign .. " = ..."
    targs = targs .. ")"
    
    argassign = argassign .. "\nreturn "
    targs = targs .. "\nreturn "
    for treeno,t in ipairs(trees) do
        local line = string.format(treetostring(t),unpack(args))
        io.write(string.format("\r%3d%% %s",treeno/#trees * 100,line))
        local lcode = argassign .. line
        local tcode = targs .. line .. " end"
        local lfn = assert(loadstring(lcode))
        local tfn = assert(terralib.loadstring(tcode))()
        tfn:compile()
        local states = 2 ^ nargs
        local actuals = {}
        local band = bit.band
        local blshift = bit.lshift
        for i = 0, states - 1 do
            for s = 1, nargs do
                actuals[s] = 0 ~= band(i,blshift(1,s-1))
            end 
            local result = lfn(unpack(actuals))
            local result2 = tfn(unpack(actuals))
            assert(result == result2)
        end
    end
    io.write("\n")
end

gentrees(2)