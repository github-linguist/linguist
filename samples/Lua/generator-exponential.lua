--could be done with a coroutine, but a simple closure works just as well.
local function powgen(m)
  local count = 0
  return function()
    count = count + 1
    return count^m
  end
end

local squares = powgen(2)
local cubes = powgen(3)

local cowrap,coyield = coroutine.wrap, coroutine.yield

local function filter(f,g)
  return cowrap(function()
    local ff,gg = f(), g()
    while true do
      if ff == gg then
        ff,gg = f(), g()
      elseif ff < gg then
        coyield(ff)
        ff = f()
      else
        gg = g()
      end
    end
  end)
end

filter = filter(squares,cubes)

for i = 1,30 do
  local result = filter()
  if i > 20 then
    print(result)
  end
end
