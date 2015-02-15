require 'matrix'

class Matrix
  # Add "permanent" method to Matrix class
  def permanent
    r = (0...row_count).to_a # [0,1] (first example), [0,1,2,3] (second example)
    r.permutation.inject(0) do |sum, sigma|
       sum += sigma.zip(r).inject(1){|prod, (row, col)| prod *= self[row, col] }
    end
  end
end

m1 = Matrix[[1,2],[3,4]] # testcases from Python version

m2 = Matrix[[1, 2, 3, 4], [4, 5, 6, 7], [7, 8, 9, 10], [10, 11, 12, 13]]

m3 = Matrix[[0, 1, 2, 3, 4],
            [5, 6, 7, 8, 9],
            [10, 11, 12, 13, 14],
            [15, 16, 17, 18, 19],
            [20, 21, 22, 23, 24]]

[m1, m2, m3].each do |m|
  puts "determinant:\t #{m.determinant}", "permanent:\t #{m.permanent}"
  puts
end
