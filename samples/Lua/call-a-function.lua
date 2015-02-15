-- Lua functions accept any number of arguments; missing arguments are nil-padded, extras are dropped.
function fixed (a, b, c) print(a, b, c) end
fixed() --> nil nil nil
fixed(1, 2, 3, 4, 5) --> 1 2 3

-- True vararg functions include a trailing ... parameter, which captures all additional arguments as a group of values.
function vararg (...) print(...) end
vararg(1, 2, 3, 4, 5) -- 1 2 3 4 5

-- Lua also allows dropping the parentheses if table or string literals are used as the sole argument
print "some string"
print { foo = "bar" } -- also serves as a form of named arguments

-- First-class functions in expression context
print(("this is backwards uppercase"):gsub("%w+", function (s) return s:upper():reverse() end))

-- Functions can return multiple values (including none), which can be counted via select()
local iter, obj, start = ipairs { 1, 2, 3 }
print(select("#", (function () end)())) --> 0
print(select("#", unpack { 1, 2, 3, 4 })) --> 4

-- Partial application
function prefix (pre)
    return function (suf) return pre .. suf end
end

local prefixed = prefix "foo"
print(prefixed "bar", prefixed "baz", prefixed "quux")

-- nil, booleans, and numbers are always passed by value. Everything else is always passed by reference.
-- There is no separate notion of subroutines
-- Built-in functions are not easily distinguishable from user-defined functions
