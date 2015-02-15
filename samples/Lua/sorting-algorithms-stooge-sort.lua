local Y = function (f)
  return (function(x) return x(x) end)(function(x) return f(function(...) return x(x)(...) end) end)
end

function stoogesort(L, pred)

  pred = pred or function(a,b) return a < b end

  Y(function(recurse)
    return function(i,j)
      if pred(L[j], L[i]) then
        L[j],L[i] = L[i],L[j]
      end
      if j - i > 1 then
        local t = math.floor((j - i + 1)/3)
        recurse(i,j-t)
        recurse(i+t,j)
        recurse(i,j-t)
      end
    end
  end)(1,#L)

  return L
end

print(unpack(stoogesort{9,7,8,5,6,3,4,2,1,0}))
