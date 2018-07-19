class Array
  def gnomesort!
    i, j = 1, 2
    while i < length
      if self[i-1] <= self[i]
        i, j = j, j+1
      else
        self[i-1], self[i] = self[i], self[i-1]
        i -= 1
        if i == 0
          i, j = j, j+1
        end
      end
    end
    self
  end
end
ary = [7,6,5,9,8,4,3,1,2,0]
ary.gnomesort!
# => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
