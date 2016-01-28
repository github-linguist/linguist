
local C = tuple(int,tuple(int,int))
terra anon()
    var c : tuple(int,tuple(int,int)) = { 1, {2,3} }
    var d : C = c
    d._0 = 2
    return d._0 + c._1._0 + c._1._1
end

test = require("test")

test.eq(anon(),7)