class Array
  def bubblesort1!
    length.times do |j|
      for i in 1...(length - j)
        if self[i] < self[i - 1]
          self[i], self[i - 1] = self[i - 1], self[i]
        end
      end
    end
    self
  end
   def bubblesort2!
    each_index do |index|
      (length - 1).downto( index ) do |i|
        self[i-1], self[i] = self[i], self[i-1] if self[i-1] < self[i]
      end
    end
    self
  end
end
ary = [3, 78, 4, 23, 6, 8, 6]
ary.bubblesort1!
p ary
# => [3, 4, 6, 6, 8, 23, 78]
