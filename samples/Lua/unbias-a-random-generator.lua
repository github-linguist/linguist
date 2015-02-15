local function randN(n)
  return function()
    if math.random() < 1/n then return 1 else return 0 end
  end
end

local function unbiased(n)
  local biased = randN (n)
  return function()
    local a, b = biased(), biased()
    while a==b do
      a, b = biased(), biased()
    end
    return a
  end
end

local function demonstrate (samples)
  for n = 3, 6 do
    biased = randN(n)
    unbias = unbiased(n)
    local bcounts = {[0]=0,[1]=0}
    local ucounts = {[0]=0,[1]=0}
    for i=1, samples do
      local bnum = biased()
      local unum = unbias()
      bcounts[bnum] = bcounts[bnum]+1
      ucounts[unum] = ucounts[unum]+1
    end
    print(string.format("N = %d",n),
      "# 0", "# 1",
      "% 0", "% 1")
    print("biased", bcounts[0], bcounts[1],
      bcounts[0] / samples * 100,
      bcounts[1] / samples * 100)
    print("unbias", ucounts[0], ucounts[1],
      ucounts[0] / samples * 100,
      ucounts[1] / samples * 100)
  end
end

demonstrate(100000)
