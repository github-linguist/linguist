class Array
  def stoogesort
    self.dup.stoogesort!
  end

  def stoogesort!(i = 0, j = self.length-1)
    if self[j] < self[i]
      self[i], self[j] = self[j], self[i]
    end
    if j - i > 1
      t = (j - i + 1)/3
      stoogesort!(i, j-t)
      stoogesort!(i+t, j)
      stoogesort!(i, j-t)
    end
    self
  end
end

p [1,4,5,3,-6,3,7,10,-2,-5].stoogesort
