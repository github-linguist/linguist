counter = 0

function test()
   print("Depth:", counter)
   counter = counter + 1
   test()
end

test()
