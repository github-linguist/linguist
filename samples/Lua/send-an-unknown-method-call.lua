local example = { }
function example:foo (x) return 42 + x end

local name = "foo"
example[name](example, 5) --> 47
