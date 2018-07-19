def longmult(x,y)
  digits = reverse_split_number(x)
  result = [0]
  j = 0
  reverse_split_number(y).each do |m|
    c = 0
    i = j
    digits.each do |d|
      v = result[i]
      result << 0 if v.zero?
      c, v = (v + c + d*m).divmod(10)
      result[i] = v
      i += 1
    end
    result[i] += c
    j += 1
  end
  # calculate the answer from the result array of digits
  result.reverse.inject(0) {|sum, n| 10*sum + n}
end

def reverse_split_number(m)
  digits = []
  while m > 0
    m, v = m.divmod 10
    digits << v
  end
  digits
end

n=2**64
printf "         %d * %d = %d\n", n, n, n*n
printf "longmult(%d, %d) = %d\n", n, n, longmult(n,n)
