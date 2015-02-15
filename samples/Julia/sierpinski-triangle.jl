pprint(matrix) = print(mapslices(x-> [join(x)], matrix, [2]))

spaces(m,n) = [" " for i=1:m, j=1:n]

function sierpinski(n)
  x = ["*" for i=1, j=1]
  for i = 1:n
    h,w = size(x)
    s = spaces(h,(w+1)/2)
    t = spaces(h,1)
    x = [[s x s] ; [x t x]]
  end
  x
end
