function combsort(t)
  local gapd, gap, swaps = 1.2473, #t, 0
  while gap + swaps > 1 do
    local k = 0
    swaps = 0
    if gap > 1 then gap = math.floor(gap / gapd) end
    for k = 1, #t - gap do
      if t[k] > t[k + gap] then
        t[k], t[k + gap], swaps = t[k + gap], t[k], swaps + 1
      end
    end
  end
  return t
end

print(unpack(combsort{3,5,1,2,7,4,8,3,6,4,1}))
