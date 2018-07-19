function qsolve(a, b, c)
  if b < 0 then return qsolve(-a, -b, -c) end
  val = b + (b^2 - 4*a*c)^(1/2) --this never exhibits instability if b > 0
  return -val / (2 * a), -2 * c / val --2c / val is the same as the "unstable" second root
end

for i = 1, 12 do
  print(qsolve(1, 0-10^i, 1))
end
