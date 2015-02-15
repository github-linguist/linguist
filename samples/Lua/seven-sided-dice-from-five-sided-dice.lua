dice5 = function() return math.random(5) end

function dice7()
  x = dice5() * 5 + dice5() - 6
  if x > 20 then return dice7() end
  return x%7 + 1
end
