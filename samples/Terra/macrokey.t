
what = function() return "abcd" end

terra foo()
  var a =   { [what()] = 4 }
  return a.abcd
end
local test = require("test")
test.eq(foo(),4)