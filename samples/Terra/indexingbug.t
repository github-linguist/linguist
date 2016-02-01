arr = terralib.new(int[256])

arr[252] = 12

terra foo(arr : &int)
  var idx = [uint8](252)
  return arr[idx]
end

terra foo2(arr : &int)
  var idx = [uint8](252)
  return @(arr + idx)
end

terra foo3(arr : &int)
  var idx = [int8](-3)
  arr = arr + 255
  return @(arr + idx)
end


terra foo4(arr : &int)
  var idx = [int8](-3)
  arr = arr + 255
  return arr[idx]
end

assert(foo2(arr) == 12)
assert(foo(arr) == 12)
assert(foo3(arr) == 12)
assert(foo4(arr) == 12)