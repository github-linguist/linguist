av, sn = math.abs, function(s) return s~=0 and s/av(s) or 0 end
function sindex(y, x) -- returns the value at (x, y) in a spiral that starts at 1 and goes outwards
  if y == -x and y >= x then return (2*y+1)^2 end
  local l = math.max(av(y), av(x))
  return (2*l-1)^2+4*l+2*l*sn(x+y)+sn(y^2-x^2)*(l-(av(y)==l and sn(y)*x or sn(x)*y)) -- OH GOD WHAT
end

function spiralt(side)
  local ret, start, stop = {}, math.floor((-side+1)/2), math.floor((side-1)/2)
  for i = 1, side do
    ret[i] = {}
    for j = 1, side do
      ret[i][j] = side^2 - sindex(stop - i + 1,start + j - 1) --moves the coordinates so (0,0) is at the center of the spiral
    end
  end
  return ret
end

for i,v in ipairs(spiralt(8)) do for j, u in ipairs(v) do io.write(u .. "   ") end print() end
