terra scope()
    var a = 4
    var b = 0
    b = b + a
    do
        var a = 5
        b = b + a
    end
    b = b + a
    return b
end
local test = require('test')
test.eq(scope(),13)