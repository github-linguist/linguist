nums = [3, 1, 4, 1, 5, 9]
nums.empty? ? 0 : nums.inject(:+) / Float(nums.size)
