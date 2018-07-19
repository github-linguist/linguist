require 'matrix'

class Matrix
  def symmetric?
    return false if not square?
    (0 ... row_size).each do |i|
      (0 .. i).each do |j|
        return false if self[i,j] != self[j,i]
      end
    end
    true
  end

  def cholesky_factor
    raise ArgumentError, "must provide symmetric matrix" unless symmetric?
    l = Array.new(row_size) {Array.new(row_size, 0)}
    (0 ... row_size).each do |k|
      (0 ... row_size).each do |i|
        if i == k
          sum = (0 .. k-1).inject(0.0) {|sum, j| sum + l[k][j] ** 2}
          val = Math.sqrt(self[k,k] - sum)
          l[k][k] = val
        elsif i > k
          sum = (0 .. k-1).inject(0.0) {|sum, j| sum + l[i][j] * l[k][j]}
          val = (self[k,i] - sum) / l[k][k]
          l[i][k] = val
        end
      end
    end
    Matrix[*l]
  end
end

puts Matrix[[25,15,-5],[15,18,0],[-5,0,11]].cholesky_factor
puts Matrix[[18, 22,  54,  42],
            [22, 70,  86,  62],
            [54, 86, 174, 134],
            [42, 62, 134, 106]].cholesky_factor
