terra foo(a : &int) : ptrdiff
  var b : &int b = a + 10
  return b-a
end

terra bar()
  var b:int = 10;
  return foo(&b)
end

terra foo2(a : &int) : &int
  a = a + 6 
  a = 3 + a
  a = a + 10
  return a
end

terra foo3(a : &int) : &int
  a = a - 11
  a = 2 + a
  a = a - 10
  return a
end

terra bar2(a : int)
  return @(foo3(foo2(&a)))
end

local test = require("test")
test.eq(bar(),10)
test.eq(bar2(42),42)
