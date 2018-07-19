function a(k,x1,x2,x3,x4,x5)
  local function b()
    k = k - 1
    return a(k,b,x1,x2,x3,x4)
  end
   if k <= 0 then return x4() + x5() else return b() end
end

function K(n)
  return function()
    return n
  end
end

print(a(10, K(1), K(-1), K(-1), K(1), K(0)))
