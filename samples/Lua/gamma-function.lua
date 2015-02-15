gamma, coeff, quad, qui, set = 0.577215664901, -0.65587807152056, -0.042002635033944, 0.16653861138228,	-0.042197734555571
function recigamma(z)
  return z + gamma * z^2 + coeff * z^3 + quad * z^4 + qui * z^5 + set * z^6
end

function gammafunc(z)
  if z == 1 then return 1
  elseif math.abs(z) <= 0.5 then return 1 / recigamma(z)
  else return (z - 1) * gammafunc(z-1)
  end
end
