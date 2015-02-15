function sumt(t, start, last) return start <= last and t[start] + sumt(t, start+1, last) or 0 end
function maxsub(ary, idx)
  local idx = idx or 1
  if not ary[idx] then return {} end
  local maxsum, last = 0, idx
  for i = idx, #ary do
    if sumt(ary, idx, i) > maxsum then maxsum, last = sumt(ary, idx, i), i end
  end
  local v = maxsub(ary, idx + 1)
  if maxsum < sumt(v, 1, #v) then return v end
  local ret = {}
  for i = idx, last do ret[#ret+1] = ary[i] end
  return ret
end
