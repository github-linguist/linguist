local C, Ct, R, Cf, Cc = lpeg.C, lpeg.Ct, lpeg.R, lpeg.Cf, lpeg.Cc
astable = Ct(C(1)^0)

function compress(t)
    local ret = {}
    for i, v in ipairs(t) do
      if t[i-1] and v == t[i-1] then
        ret[#ret - 1] = ret[#ret - 1] + 1
      else
        ret[#ret + 1] = 1
        ret[#ret + 1] = v
      end
    end
    t = ret
    return table.concat(ret)
end
q = io.read()
print(compress(astable:match(q)))

undo = Ct((Cf(Cc"0" * C(R"09")^1, function(a, b) return 10 * a + b end) * C(R"AZ"))^0)

function decompress(s)
  t = undo:match(s)
  local ret = ""
  for i = 1, #t - 1, 2 do
    for _ = 1, t[i] do
      ret = ret .. t[i+1]
    end
  end
  return ret
end
