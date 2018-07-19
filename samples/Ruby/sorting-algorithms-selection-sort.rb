class Array
  def selectionsort!
    for i in 0..length-2
      min_idx = i
      for j in (i+1)...length
        min_idx = j  if self[j] < self[min_idx]
      end
      self[i], self[min_idx] = self[min_idx], self[i]
    end
    self
  end
end
ary = [7,6,5,9,8,4,3,1,2,0]
p ary.selectionsort!
# => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
