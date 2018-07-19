class Array
  def strandsort
    a = self.dup
    result = []
    until a.empty?
      sublist = [a.shift]
      a.each_with_index.each_with_object([]) { |(val, idx), remove|
        next if val <= sublist.last
        sublist << val
        remove << idx
      }.reverse_each {|idx| a.delete_at(idx)}

      result.each_index do |idx|
        break if sublist.empty?
        result.insert(idx, sublist.shift) if sublist[0] < result[idx]
      end
      result += sublist
    end
    result
  end

  def strandsort!
    replace(strandsort)
  end
end

p [1, 6, 3, 2, 1, 7, 5, 3].strandsort
