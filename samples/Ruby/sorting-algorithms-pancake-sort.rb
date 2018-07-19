class Array
  def pancake_sort!
    num_flips = 0
    (self.size-1).downto(1) do |end_idx|
      max     = self[0..end_idx].max
      max_idx = self[0..end_idx].index(max)
      next if max_idx == end_idx

      if max_idx > 0
        self[0..max_idx] = self[0..max_idx].reverse
        p [num_flips += 1, self]  if $DEBUG
      end

      self[0..end_idx] = self[0..end_idx].reverse
      p [num_flips += 1, self]  if $DEBUG
    end
    self
  end
end

p a = (1..9).to_a.shuffle
p a.pancake_sort!
