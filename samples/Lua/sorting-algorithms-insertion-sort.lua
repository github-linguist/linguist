function bins(tb, val, st, en)
  local st, en = st or 1, en or #tb
  local mid = math.floor((st + en)/2)
  if en == st then return tb[st] > val and st or st+1
  else return tb[mid] > val and bins(tb, val, st, mid) or bins(tb, val, mid+1, en)
  end
end
function isort(t)
  local ret = {t[1], t[2]}
  for i = 3, #t do
    table.insert(ret, bins(ret, t[i]), t[i])
  end
  return ret
end

print(unpack(isort{4,5,2,7,8,3}))
