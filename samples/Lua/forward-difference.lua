function dif(a, b, ...)
  if(b) then return b-a, dif(b, ...) end
end
function dift(t) return {dif(unpack(t))} end
print(unpack(dift{1,3,6,10,15}))
